package ircbot
package db

import scala.slick.driver.MySQLDriver.simple._
import org.joda.time.DateTime
import Helpers._

case class Factoid(token: String,
                   description: String,
                   dateLastEdit: DateTime = new DateTime,
                   hits: Int = 0,
                   idUser: Int = 0) {
}

class Factoids(t: Tag) extends Table[Factoid](t, "irc_factoids") {
  def token                 = column[String]("token", O.PrimaryKey)
  def description           = column[String]("description")
  def dateLastEdit          = column[DateTime]("date_lastedit")
  def hits                  = column[Int]("hits")
  def idUser                = column[Int]("id_user")

  def * = (token, description, dateLastEdit, hits, idUser) <> (Factoid.tupled, Factoid.unapply _)
}
