package ircbot
package db

import scala.slick.driver.MySQLDriver.simple._
import org.joda.time.DateTime

object Helpers {

  implicit val yesNoTypeMapper = MappedColumnType.base[Boolean, String](
    { b => if(b)   "yes" else "no" },
    { s => s == "yes" }
  )

  implicit val jodaToSql = MappedColumnType.base[DateTime, java.sql.Timestamp](
    { dt => new java.sql.Timestamp(dt.getMillis()) },
    { ts => new DateTime(ts.getTime) }
  )

  val factoids = TableQuery[Factoids]
  val users = TableQuery[Users]

  def factoidByToken(name: String) = {
    factoids.filter(_.token === name)
  }

  implicit def userlevelMapper = MappedColumnType.base[UserLevel, String](
    { _ match {
      case Guest => "guest"
      case Regular => "regular"
      case Administrator => "administrator"
      case Manager => "manager"
    } },
    { _ match {
      case "guest" => Guest
      case "regular"=> Regular
      case "administrator" => Administrator
      case "manager" => Manager
    } }
  )
}
