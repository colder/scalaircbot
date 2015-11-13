package ircbot
package db

import slick.driver.MySQLDriver.api._
import org.joda.time.{DateTime, Duration}
import Helpers._
import BanTypes._

case class BanLog(id: Option[Int],
                  accountAdmin: String,
                  accountUser: String,
                  tpe: BanType,
                  dateStart: DateTime,
                  duration: Duration,
                  dateEnd: Option[DateTime],
                  reason: String) {

  def isCompleted = dateEnd.isDefined

  def isPermanent = duration.getMillis < 0

  def remaining: Duration = dateEnd match {
    case Some(e) =>
      new Duration(0) 
    case None =>
      new Duration(new DateTime(), dateStart.plus(duration))
  }
}

object BanTypes {
  abstract class BanType
  case object Ban extends BanType
  case object Mute extends BanType
}

class BanLogs(t: Tag) extends Table[BanLog](t, "irc_banlog") {
  def id               = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def accountAdmin     = column[String]("account_admin")
  def accountUser      = column[String]("account_user")
  def tpe              = column[BanType]("type")
  def dateStart        = column[DateTime]("date_start")
  def duration         = column[Duration]("duration")
  def dateEnd          = column[Option[DateTime]]("date_end")
  def reason           = column[String]("reason")

  def * = (id.?, accountAdmin, accountUser, tpe, dateStart, duration, dateEnd, reason) <> (BanLog.tupled, BanLog.unapply _)
}
