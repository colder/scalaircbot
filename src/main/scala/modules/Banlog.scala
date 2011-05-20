package ircbot
package modules

import utils._

import java.util.{Date,Calendar}
import java.text.SimpleDateFormat

abstract class BanType {
  val alt: String
}
case object Ban  extends BanType {
  override def toString = "ban"
  val alt = "banned"
}
case object Mute extends BanType {
  override def toString = "mute"
  val alt = "muted"
}

case class BanLogEntry(id: Int = 0, banner: Ident, banned: Ident, tpe: BanType, dateStart: Date, duration: Duration, dateEnd: Option[Date], reason: String) {
  def expectedDateEnd = {
    val cal   = Calendar.getInstance()

    cal.setTime(dateStart)
    cal.add(Calendar.SECOND, duration.toSeconds)

    cal.getTime()

  }

  def hasExpired = {
    expectedDateEnd.before(new Date())
  }

  lazy val explainString = {
    val end = dateEnd match {
      case Some(de) =>
        Helpers.dateAsString(de)+"     "
      case None =>
        Helpers.dateAsString(expectedDateEnd)+" (exp)"
    }

    "["+(if (tpe == Mute) "mute" else "ban ")+"] at "+Helpers.dateAsString(dateStart)+" by "+banner.value+" until "+end +". Reason: "+reason
  }
}

class BanLog(val ctl: Control) extends Module(ctl) with Commands {
  val channel = Channel("##php")

  var banLog = Set[BanLogEntry]()

  override def init = {
    loadBanLog
  }

  def handleMessage(msg: Message) = {
    checkBans
    msg match {
      case Msg(from, to, msg) =>
        words(msg, 4) match {
          case "!ban" :: nick :: ExDuration(d) :: reason :: Nil  =>
            requireAuth(from, Manager, Administrator) {
              registerBan(from, Nick(nick), d, reason)
            }
            false

          case "!mute" :: nick :: ExDuration(d) :: reason :: Nil =>
            requireAuth(from, Manager, Administrator) {
              registerMute(from, Nick(nick), d, reason)
            }
            false

          case "!unban" :: nick :: Nil  =>
            requireAuth(from, Manager, Administrator) {
              registerUnban(from, Nick(nick))
            }
            false

          case "!unmute" :: nick :: Nil =>
            requireAuth(from, Manager, Administrator) {
              registerUnmute(from, Nick(nick))
            }
            false


          case "!banstatus" :: Nil =>
            requireAuth(from, Manager, Administrator) {
              val activeEntries = banLog.filter(_.dateEnd.isEmpty)

              if (activeEntries.isEmpty) {
                ctl.p.msg(from.nick, "Nobody is currently being banned/muted.")
              } else {
                ctl.p.msg(from.nick, "Banlist ("+activeEntries.size+"): "+activeEntries.map(ae =>  ae.tpe+" "+ae.banned.value+" until "+Helpers.dateAsString(ae.expectedDateEnd)).mkString(", "))
              }
            }
            false

          case "!banexplain" :: nick :: Nil =>
            requireAuth(from, Manager, Administrator) {
              ctl.idents.getIdent(Nick(nick)) match {
                case Some(ident) =>
                  val entries = banLog.filter(_.banned == ident)

                  if (entries.isEmpty) {
                    ctl.p.msg(from.nick, "No ban/mute has been ever registered against account "+ident.value+"")
                  } else {
                    ctl.p.msg(from.nick, "Listing "+entries.size+" relevant ban entries:")
                    for (e <- entries) {
                      ctl.p.msg(from.nick, " "+e.explainString)
                    }
                  }
                case None =>
                  ctl.p.msg(from.nick, "Cannot find an account attached to that nickname.")
              }
            }
            false

          case "!ban" :: _ | "!mute" :: _ | "!unban" :: _ | "!unmute" :: _  | "!banexplain" :: _ | "!banstatus" :: _ =>
            requireAuth(from, Manager, Administrator) {
              banLogHelp(from)
            }
            false
          case _ =>
            true
        }
      case _ =>
        true
    }
  }

  private def banLogHelp(from: Prefix) {
    ctl.p.msg(from.nick, "Invalid ban command. !ban/!mute <nick> <duration (1d | 2h | 3m)> <reason> || !banstatus || !banexplain <nick> || !unban/!unmute <nick>")
  }

  def registerBan(from: Prefix, who: UserID, duration: Duration, reason: String) = registerBanEx(Some(from), Ban, who, duration, reason)
  def registerMute(from: Prefix, who: UserID, duration: Duration, reason: String) = registerBanEx(Some(from), Mute, who, duration, reason)

  def registerBanSilent(who: UserID, duration: Duration, reason: String) = registerBanEx(None, Ban, who, duration, reason)
  def registerMuteSilent(who: UserID, duration: Duration, reason: String) = registerBanEx(None, Mute, who, duration, reason)

  private def registerBanEx(from: Option[Prefix], tpe: BanType, who: UserID, duration: Duration, reason: String) = {
    def optmsg(msg: String) = from match {
      case Some(prefix) =>
        ctl.p.msg(prefix.nick, msg)
      case None =>
        ctl.l.info(msg)
    }

    // 1) Get idents
    val oidentbanned = who match {
      case i: Ident =>
        Some(i)
      case n: Nick =>
        ctl.factoids.lookup("banmessage") match {
          case Some(banmessage) =>
            // Notify the user by PM:
            try {
              ctl.p.msg(n, String.format(banmessage, tpe.alt, duration.toString, reason))
            } catch {
              case e =>
                ctl.error("Cound not format: "+e.getMessage)
            }
          case None =>
            ctl.error("Cound not find factoid banmessage!")
        }

        ctl.idents.getIdent(n)
    }

    val oidentbanner = from match {
      case Some(pr) =>
        ctl.idents.getIdent(pr.nick)
      case None =>
        Some(ctl.cfg.authIdent)
    }

    (oidentbanned, oidentbanner) match {
      case (Some(identbanned), Some(identbanner)) =>
        val newBanEntry = BanLogEntry(0, identbanner, identbanned, tpe, new Date(), duration, None, reason)

        // 2) Check for existing ban/mute
        banLog.find(be => be.banned == identbanned && be.tpe == tpe && be.dateEnd.isEmpty) match {
          case Some(banEntry) =>
            val term = if (banEntry.expectedDateEnd.after(newBanEntry.expectedDateEnd)) {
              "after"
            } else {
              "before"
            }

            optmsg("Found an existing "+tpe+" entry for account "+identbanned.value+" that expires "+term+" this one! Replaced entry with the new.")

            banLog -= banEntry
            storeBanEntry(banEntry.copy(dateEnd = Some(new Date())))


          case None =>
            ctl.chanserv.doAsOP(channel) {
              if (newBanEntry.tpe == Mute) {
                ctl.p.mute(channel, newBanEntry.banned.toMask)
              } else {
                ctl.p.ban(channel, newBanEntry.banned.toMask)
              }
            }
        }

        storeBanEntry(newBanEntry)

        optmsg(newBanEntry.tpe.toString.capitalize+" for account "+identbanned.value+" in place for "+duration+", and logged.")

      case (None, _) =>
        optmsg("Could not find any ident associated with "+who)
      case (_, None) =>
        optmsg("Could not find any ident associated with the banner !?!")
    }
  }

  def registerUnban(from: Prefix, who: UserID) = registerUnbanEx(from, Ban, who)
  def registerUnmute(from: Prefix, who: UserID) = registerUnbanEx(from, Mute, who)

  private def registerUnbanEx(from: Prefix, tpe: BanType, who: UserID) = {
    // 1) Get idents
    val oidentbanned = who match {
      case i: Ident =>
        Some(i)
      case n: Nick =>
        ctl.idents.getIdent(n)
    }

    val oidentbanner = ctl.idents.getIdent(from.nick)

    (oidentbanned, oidentbanner) match {
      case (Some(identbanned), Some(identbanner)) =>
        // 1) Check for existing ban/mute
        banLog.find(be => be.banned == identbanned && be.tpe == tpe && be.dateEnd.isEmpty) match {
          case Some(banEntry) =>
            banLog -= banEntry
            storeBanEntry(banEntry.copy(dateEnd = Some(new Date())))

            ctl.chanserv.doAsOP(channel) {
              if (tpe == Mute) {
                ctl.p.unmute(channel, identbanned.toMask)
              } else {
                ctl.p.unban(channel, identbanned.toMask)
              }
            }

            ctl.p.msg(from.nick, "Ban for account "+identbanned.value+" removed.")

          case None =>
            ctl.p.msg(from.nick, "No active "+tpe+" entry found against "+identbanned.value+".")
        }


      case (None, _) =>
        ctl.p.msg(from.nick, "Could not find any ident associated with "+who)
      case (_, None) =>
        ctl.p.msg(from.nick, "Could not find any ident associated with the banner !?!")
    }
  }

  private def checkBans = {
    for (be <- banLog.filter(bl => bl.dateEnd.isEmpty && bl.hasExpired)) {
      // People to unban
      ctl.chanserv.doAsOP(channel) {
        if (be.tpe == Mute) {
          ctl.p.unmute(channel, be.banned.toMask)
        } else {
          ctl.p.unban(channel, be.banned.toMask)
        }
      }

      banLog -= be

      val newBE = be.copy(dateEnd = Some(new Date()))

      storeBanEntry(newBE)
    }
  }

  private def loadBanLog = {
    try {
      val stmt = ctl.db.prepareStatement("SELECT id, account_admin, account_user, type, date_start, duration, date_end, reason FROM irc_banlog")
      val results = stmt.executeQuery

      results.foreach { row =>
          banLog += BanLogEntry(
              id        = row.getInt("id"),
              banner    = Ident(row.getString("account_admin")),
              banned    = Ident(row.getString("account_user")),
              tpe       = if (row.getString("account_user") == "mute") Mute else Ban,
              dateStart = row.getTimestamp("date_start"),
              duration  = Seconds(row.getInt("duration")),
              dateEnd   = if (row.getDate("date_end") eq null) None else Some(row.getTimestamp("date_end")),
              reason    = row.getString("reason")
          )
      }

      stmt.close
    } catch {
        case ex: Exception =>
            ctl.db.handleException(ex)
    }
  }

  private def storeBanEntry(banEntry: BanLogEntry): BanLogEntry = {
    val sql_shared = "irc_banlog SET account_admin = ?, account_user = ?, type = ?, date_start = ?, duration = ?, date_end = ?, reason = ?"


    val newBanEntry = if (banEntry.id == 0) {
      // insert
      val stmt = ctl.db.prepareStatement("INSERT INTO "+sql_shared, banEntry.banner.value,
                                                                    banEntry.banned.value,
                                                                    banEntry.tpe.toString,
                                                                    banEntry.dateStart,
                                                                    banEntry.duration.toSeconds,
                                                                    banEntry.dateEnd,
                                                                    banEntry.reason)
      stmt.executeUpdate

      banEntry.copy(id = ctl.db.lastInsertID)
    } else {
      // update
      val stmt = ctl.db.prepareStatement("UPDATE "+sql_shared+" WHERE id = ?", banEntry.banner.value,
                                                                    banEntry.banned.value,
                                                                    banEntry.tpe.toString,
                                                                    banEntry.dateStart,
                                                                    banEntry.duration.toSeconds,
                                                                    banEntry.dateEnd,
                                                                    banEntry.reason,
                                                                    banEntry.id)
      stmt.executeUpdate

      banEntry
    }

    banLog += newBanEntry

    newBanEntry
  }
}
