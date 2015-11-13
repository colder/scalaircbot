package ircbot
package modules

import akka.actor._
import org.joda.time.{DateTime, Duration}
import org.joda.time.format._

import slick.driver.MySQLDriver.api._
import utils._
import InnerProtocol._

import db.BanTypes._
import db.Helpers._
import db.User
import db.{BanLog => DBBanLog, BanLogs => DBBanLogs}

class BanLog(val db: Database,
             val ctl: ActorRef,
             val chan: Channel) extends Module {

  override def receive = {
    case From(NickMask(admin), Msg(to: Nick, msg)) =>
      words(msg, 4) match {
        case "!ban"    :: nick :: duration :: reason :: Nil =>
          requireGranted(admin, Administrator) {
            (getUser(admin) zip getUser(Nick(nick))).collect { case (Some(adminAccount), Some(userAccount)) =>
              parseDuration(duration) match {
                case Some(d) =>
                  doBan(Some(admin), Ban, adminAccount, userAccount, d, reason)
                case None =>
                  send(Msg(admin, s"Unable to extract duration '$duration'"))
              }
            }
          }
        case "!mute"   :: nick :: duration :: reason :: Nil =>
          requireGranted(admin, Administrator) {
            (getUser(admin) zip getUser(Nick(nick))).collect { case (Some(adminAccount), Some(userAccount)) =>
              parseDuration(duration) match {
                case Some(d) =>
                  doBan(Some(admin), Mute, adminAccount, userAccount, d, reason)
                case None =>
                  send(Msg(admin, s"Unable to extract duration '$duration'"))

              }
            }
          }
        case ("!unban" | "!unmute")  :: nick :: Nil =>
          requireGranted(admin, Administrator) {
            getUser(Nick(nick)).collect { case Some(userAccount) =>
              doUnban(admin, userAccount)
            }
          }

        case "!banstatus" :: nick :: Nil =>
          requireGranted(admin, Administrator) {
            getUser(Nick(nick)).collect { case Some(userAccount) =>
              banHistory(admin, userAccount)
            }
          }
        case "!banstatus" :: Nil =>
          requireGranted(admin, Administrator) {
            banStatus(admin)
          }

        case _ =>
      }

    case RequestBan(tpe, nick, duration, reason) =>
      println("Got "+tpe+" request")
      getUser(nick).collect { case Some(userAccount) =>
        doBan(None, tpe, User("php-bot", Manager), userAccount, duration, reason)
      }

    case Tick =>
      removeElapsedBans


    case msg =>
      super.receive(msg)

  }

  def durationToString(d: Duration): String = {
    var s = d.getMillis/1000
    val days  = s / (60*60*24)
    s = s % (60*60*24)
    val hours = s / (60*60)
    s = s % (60*60)
    val mins = s / (60)
    s = s % (60)

    def n(amount: Long, unit: String): Option[String] = {
      if (amount > 0) {
        Some(amount+unit)
      } else {
        None
      }
    }

    List(n(days, "d"), n(hours, "h"), n(mins, "m")).flatten.mkString(" ")
  }


  val dateFormat = DateTimeFormat.forPattern("dd.MM.YYYY HH:mm")

  def dateToString(d: DateTime): String = {
    dateFormat.print(d)
  }

  def parseDuration(s: String): Option[Duration] = s match {
    case "perm" | "permanent" => Some(new Duration(-1000))
    case s => try {
      val f = new PeriodFormatterBuilder()
       .appendDays()
       .appendSuffix("d")
       .appendSeparator(" ")
       .appendHours()
       .appendSuffix("h")
       .appendSeparator(" ")
       .appendMinutes()
       .appendSuffix("m")
       .toFormatter();

       Some(f.parsePeriod(s).toDurationFrom(now()))
    } catch {
      case _: Exception => None
    }
  }

  def doBan(ofrom: Option[Nick], tpe: BanType, admin: User, user: User, duration: Duration, reason: String) {
    val q = getExistingBan(user)

    for (ob <- db.run(q.result.headOption)) {
      ob match {
        case Some(b) =>
          val nb = b.copy(duration = duration, tpe = tpe, reason = reason, dateStart = now(), accountAdmin = admin.account)
          ofrom.foreach { from =>
            send(Msg(from, "Updated entry:"))
            send(Msg(from, getBanDesc(nb)))
          }


          db.run(q.update(nb))
        case None =>
          requireOP(chan) {
            val nb = DBBanLog(None, admin.account, user.account, tpe, now(), duration, None, reason)
            ofrom.foreach { from =>
              send(Msg(from, "New entry:"))
              send(Msg(from, getBanDesc(nb)))
            }
            db.run(banlogs += nb)

            // Do the ban
            import Modes._
            val m = if (tpe == Mute) {
              Q(AccountMask(user.account))
            } else {
              B(AccountMask(user.account))
            }
            send(Mode(chan, List(Plus(m))))
          }
      }
    }
  }

  def doUnban(from: Nick, user: User) {
    val q = getExistingBan(user)
    for (ob <- db.run(q.result.headOption)) {
      ob match {
        case Some(b) =>
          requireOP(chan) {
            import Modes._
            val nb = b.copy(dateEnd = Some(now()))
            send(Msg(from, "Removed "+b.tpe+" for "+user.account))
            db.run(q.update(nb))

            val m = if (b.tpe == Mute) {
              Q(AccountMask(user.account))
            } else {
              B(AccountMask(user.account))
            }
            send(Mode(chan, List(Minus(m))))
          }
        case None =>
          send(Msg(from, "No entry found for account "+user.account))
      }
    }
  }

  def getBanDesc(b: DBBanLog): String = {
    val end = if (b.isCompleted) {
      "-> "+dateToString(b.dateEnd.get)
    } else if (b.isPermanent) {
      "-> permanent"
    } else {
      "-> +"+durationToString(b.remaining)+""
    }

    f" ${b.tpe}%-4s ${b.accountUser}%-20s ${dateToString(b.dateStart)} $end%-25s  Reason: ${b.reason}"
  }

  def getExistingBan(user: User): Query[DBBanLogs, DBBanLog, Seq] = {
    for {
      b <- banlogs if b.dateEnd.isEmpty && b.accountUser === user.account
    } yield (b)
  }

  def banHistory(from: Nick, user: User) {
    val q = banlogs.filter(_.accountUser === user.account).sortBy(_.dateStart)
    for (results <- db.run(q.result)) {
      results.foreach { b =>
        send(Msg(from, getBanDesc(b)))
      }

      if (results.isEmpty) {
        send(Msg(from, "No ban entry found for account "+user.account))
      }
    }
  }

  def banStatus(from: Nick){
    val q = banlogs.filter(_.dateEnd.isEmpty).sortBy(_.dateStart.desc)
    for (results <- db.run(q.result)) {
      results.take(10).foreach { b =>
        send(Msg(from, getBanDesc(b)))
      }
      if (results.size > 10) {
        send(Msg(from, s"...${results.size} more"))
      } else if (results.isEmpty) {
        send(Msg(from, "No active ban entry found"))
      }
    }
  }

  def removeElapsedBans {
    val q = banlogs.filter(_.dateEnd.isEmpty)

    for (bs <- db.run(q.result); b <- bs) {
      if (!b.isPermanent && b.remaining.getMillis < 0) {
        val q = banlogs.filter(_.id === b.id).map(_.dateEnd)
        db.run(q.update(Some(now())))

        logInfo("Removed "+b.tpe+" for "+b.accountUser)

        import Modes._
        val m = if (b.tpe == Mute) {
          Q(AccountMask(b.accountUser))
        } else {
          B(AccountMask(b.accountUser))
        }
        send(Mode(chan, List(Minus(m))))
      }
    }
  }

  override def helpEntries = List(
    HelpEntry(Administrator, "ban",        "!ban <nick> <duration> <reason>",  "Ban nick <nick> for <duration>, log <reason>"),
    HelpEntry(Administrator, "mute",       "!mute <nick> <duration> <reason>", "Mute nick <nick> for <duration>, log <reason>"),
    HelpEntry(Administrator, "unban",      "!unban <nick>",                    "Unban nick <nick>"),
    HelpEntry(Administrator, "unmute",     "!unmute <nick>",                   "Unmute nick <nick>"),
    HelpEntry(Administrator, "banstatus",  "!banstatus [<nick>]",              "Display list of active bans/mutes")
  )
}
