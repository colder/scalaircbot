package ircbot
package modules

import akka.actor._
import org.joda.time.{DateTime, Duration, PeriodType}
import org.joda.time.format._

import scala.slick.driver.MySQLDriver.simple._
import scala.concurrent.ExecutionContext.Implicits.global
import utils._
import InnerProtocol._

import db.BanTypes._
import db.Helpers._
import db.User
import db.{BanLog => DBBanLog}

class BanLog(val db: Database,
             val ctl: ActorRef) extends Module {

  override def receive = {
    case ReceivedMessage(From(NickMask(admin), Msg(to: Nick, msg))) =>
      getUser(admin).foreach {
        case Some(adminAccount) =>
          if (adminAccount.userLevel >= Administrator) {
            words(msg, 4) match {
              case "!ban"    :: nick :: duration :: reason :: Nil =>
                getUser(Nick(nick)).collect { case Some(userAccount) =>
                  doBan(admin, Ban,  adminAccount, userAccount, Duration.parse(duration), reason)
                }
              case "!mute"   :: nick :: duration :: reason :: Nil =>
                getUser(Nick(nick)).collect { case Some(userAccount) =>
                  doBan(admin, Mute, adminAccount, userAccount, Duration.parse(duration), reason)
                }
              case "!unban"  :: nick :: Nil =>
                getUser(Nick(nick)).collect { case Some(userAccount) =>
                  doUnban(admin, Ban, userAccount)
                }
              case "!unmute" :: nick :: Nil =>
                getUser(Nick(nick)).collect { case Some(userAccount) =>
                  doUnban(admin, Mute, userAccount)
                }
              case "!banstatus" :: nick :: Nil =>
                getUser(Nick(nick)).collect { case Some(userAccount) =>
                  banHistory(admin, userAccount)
                }
              case "!banstatus" :: Nil =>
                banStatus(admin)

              case _ =>
            }
          } else {
            send(Msg(admin, "Permission denied: this command requires at least the "+Administrator+" right!"))
          }
        case None =>
      }

    case msg =>
      super.receive(msg)

  }

  val durFormat = new PeriodFormatterBuilder()
    .printZeroNever()
    .appendYears()
    .appendSuffix(" year", " years")
    .appendSeparator(" ")
    .appendMonths()
    .appendSuffix(" month", " months")
    .appendSeparator(" ")
    .appendDays()
    .appendSuffix(" day", " days")
    .appendSeparator(" ")
    .appendHours()
    .appendSuffix(" hour", " hours")
    .appendSeparator(" ")
    .appendMinutes()
    .appendSuffix(" minute", " minutes")
    .toFormatter()

  val dateFormat = DateTimeFormat.forPattern("dd.MM.yyyy HH:mm")

  def durationToString(d: Duration): String = {
    durFormat.print(d.toPeriod(PeriodType.yearMonthDayTime()))
  }

  def dateToString(d: DateTime): String = {
    dateFormat.print(d)
  }

  def doBan(from: Nick, tpe: BanType, admin: User, user: User, duration: Duration, reason: String) {

  }

  def doUnban(from: Nick, tpe: BanType, user: User) {

  }

  def getBanDesc(b: DBBanLog): String = {
    if (b.isCompleted) {
      f" ${b.tpe}%-4s ${dateToString(b.dateStart)} to ${dateToString(b.dateEnd.get)}   Reason: ${b.reason}"
    } else if (b.isPermanent) {
      f" ${b.tpe}%-4s ${dateToString(b.dateStart)}... (${"permanent)"}%-30s   Reason: ${b.reason}"
    } else {
      f" ${b.tpe}%-4s ${dateToString(b.dateStart)}... (${durationToString(b.remaining)+" left)"}%-30s   Reason: ${b.reason}"
    }
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
        println(b.duration)
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
