package ircbot
package modules

import akka.actor._
import org.joda.time.{DateTime, Duration, PeriodType, Period}
import org.joda.time.format._

import scala.slick.driver.MySQLDriver.simple._
import scala.concurrent.ExecutionContext.Implicits.global
import utils._
import InnerProtocol._

import db.BanTypes._
import db.Helpers._
import db.User
import db.{BanLog => DBBanLog, BanLogs => DBBanLogs}

class BanLog(val db: Database,
             val ctl: ActorRef) extends Module {

  override def receive = {
    case ReceivedMessage(From(NickMask(admin), Msg(to: Nick, msg))) =>
      words(msg, 4) match {
        case "!ban"    :: nick :: duration :: reason :: Nil =>
          requireGranted(admin, Administrator) {
            (getUser(admin) zip getUser(Nick(nick))).collect { case (Some(adminAccount), Some(userAccount)) =>
              parseDuration(duration) match {
                case Some(d) =>
                  doBan(admin, Ban,  adminAccount, userAccount, d, reason)
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
                  doBan(admin, Ban,  adminAccount, userAccount, d, reason)
                case None =>
                  send(Msg(admin, s"Unable to extract duration '$duration'"))

              }
            }
          }
        case "!unban"  :: nick :: Nil =>
          requireGranted(admin, Administrator) {
            getUser(Nick(nick)).collect { case Some(userAccount) =>
              doUnban(admin, Ban, userAccount)
            }
          }
        case "!unmute" :: nick :: Nil =>
          requireGranted(admin, Administrator) {
            getUser(Nick(nick)).collect { case Some(userAccount) =>
              doUnban(admin, Mute, userAccount)
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
      Some(Duration.parse(s))
    } catch {
      case _: Exception => None
    }
  }

  def doBan(from: Nick, tpe: BanType, admin: User, user: User, duration: Duration, reason: String) {
    println("YAY")
    db.withSession { implicit s =>
      val q = getExistingQ(user)
    println("YAY2")
      q.firstOption match {
        case Some(b) =>
    println("YAY4")
          val nb = b.copy(duration = duration, tpe = tpe, reason = reason, dateStart = new DateTime(), accountAdmin = admin.account)
          send(Msg(from, "Updated entry:"))
          send(Msg(from, getBanDesc(nb)))
          q.update(nb)
        case None =>
    println("YAY6")
          val nb = DBBanLog(None, admin.account, user.account, tpe, new DateTime(), duration, None, reason)
          send(Msg(from, "New entry:"))
          send(Msg(from, getBanDesc(nb)))
          banlogs += nb
      }
    }
  }

  def doUnban(from: Nick, tpe: BanType, user: User) {
    db.withSession { implicit s =>
      val q = getExistingQ(user)
      q.firstOption match {
        case Some(b) =>
          val nb = b.copy(dateEnd = Some(new DateTime()))
          send(Msg(from, "Removed "+tpe+" for "+user.account))
          q.update(nb)
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

  def getExistingQ(user: User): Query[DBBanLogs, DBBanLog] = {
    for {
      b <- banlogs if b.dateEnd.isNull && b.accountUser === user.account
    } yield (b)
  }

  def banHistory(from: Nick, user: User) {
    db.withSession { implicit s =>
      val results = banlogs.filter(_.accountUser === user.account).sortBy(_.dateStart).list
      results.foreach { b =>
        send(Msg(from, getBanDesc(b)))
      }

      if (results.isEmpty) {
        send(Msg(from, "No ban entry found for account "+user.account))
      }
    }
  }

  def banStatus(from: Nick){
    db.withSession { implicit s =>
      val results = banlogs.filter(_.dateEnd.isNull).sortBy(_.dateStart.desc).list
      results.take(10).foreach { b =>
        send(Msg(from, getBanDesc(b)))
      }
      if (results.size > 10) {
        send(Msg(from, s"...${results.size} more"))
      }

      if (results.isEmpty) {
        send(Msg(from, "No active ban entry found"))
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
