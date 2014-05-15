package ircbot
package db

import scala.slick.driver.MySQLDriver.simple._
import org.joda.time.DateTime
import Helpers._

case class User(account: String,
                userLevel: UserLevel) {
}

class Users(t: Tag) extends Table[User](t, "irc_users") {
  implicit val userlevelMapper = MappedColumnType.base[UserLevel, String](
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

  def account               = column[String]("account", O.PrimaryKey)
  def userLevel             = column[UserLevel]("level")

  def * = (account, userLevel) <> (User.tupled, User.unapply _)
}
