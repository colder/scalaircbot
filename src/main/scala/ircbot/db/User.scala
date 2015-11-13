package ircbot
package db

import slick.driver.MySQLDriver.api._
import org.joda.time.DateTime
import Helpers._

case class User(account: String,
                userLevel: UserLevel) {
}

class Users(t: Tag) extends Table[User](t, "irc_users") {
  def account               = column[String]("account", O.PrimaryKey)
  def userLevel             = column[UserLevel]("level")

  def * = (account, userLevel) <> (User.tupled, User.unapply _)
}
