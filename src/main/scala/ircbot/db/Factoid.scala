package ircbot
package db

import scala.slick.driver.MySQLDriver.simple._
import org.joda.time.DateTime
import Helpers._

case class Factoid(token: String,
                   kind: FactoidKinds.Kind,
                   description: String,
                   dateLastEdit: DateTime,
                   userDefined: String,
                   userLastEdit: String,
                   hits: Int = 0) {
}

class Factoids(t: Tag) extends Table[Factoid](t, "irc_factoids") {
  def token                 = column[String]("token", O.PrimaryKey)
  def kind                  = column[FactoidKinds.Kind]("kind")
  def description           = column[String]("description")
  def dateLastEdit          = column[DateTime]("date_lastedit")
  def hits                  = column[Int]("hits")
  def userDefined           = column[String]("user_defined")
  def userLastEdit          = column[String]("user_lastedit")

  def * = (token, kind, description, dateLastEdit, userDefined, userLastEdit, hits) <> (Factoid.tupled, Factoid.unapply _)
}

object FactoidKinds {
  abstract class Kind
  case object Doc extends Kind
  case object User extends Kind
  case object Internal extends Kind
}
