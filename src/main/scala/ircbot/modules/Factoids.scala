package ircbot
package modules

import akka.actor._
import utils._
import InnerProtocol._
import scala.slick.driver.MySQLDriver.simple._
import org.joda.time.DateTime

import db.Helpers._
import db._

class Factoids(val db: Database,
               val ctl: ActorRef) extends SimpleModule {

  def onMessage(msg: Message) = msg match {
    // Public
    case From(NickMask(nick), Msg(chan: Channel, msg)) =>
      if (msg.contains("!+")) {
        msg.split("[:, ]? ?!\\+", 2).toList match {
          case prefix :: fact :: Nil =>
            ifGranted(nick, Regular) {
              lookup(fact).foreach { factoid =>
                send(Msg(chan, prefix+", "+factoid))
              }
            } {
              send(Msg(nick, "This public command can only be used by regulars. You can simply msg me: /msg php-bot "+fact))
            }
          case xs =>
        }
      }

    // Private
    case From(NickMask(nick), Msg(to: Nick, msg)) if to != nick =>
      words(msg, 2) match {
        case "!def" :: rest :: Nil =>
          rest.split("=", 2).toList match {
            case fact :: description :: Nil =>
              requireGranted(nick, Regular) {
                define(nick, fact.trim, description.trim)
                send(Msg(nick, "Defined factoid "+fact.trim))
              }
            case _ =>
              send(Msg(nick, "wat"))
          }

        case "!undef" :: fact :: Nil =>
          requireGranted(nick, Regular) {
            undefine(fact)
            send(Msg(nick, "Removed factoid "+fact))
          }
        case "!search" :: fact :: Nil =>
          requireGranted(nick, Regular) {
            search(fact) match {
              case fact :: Nil =>
                send(Msg(nick, "Found 1 result: "+fact.token+" := "+fact.description))

              case Nil =>
                send(Msg(nick, "Found 0 result"))

              case facts =>
              send(Msg(nick, "Found "+facts.size+" results: "+facts.map(_.token).take(10).mkString(", ")+(if(facts.size > 10) "..." else "")))
            }
          }
        case _ if !msg.startsWith("!") =>
          lookup(msg).orElse(Some("?")).foreach { factoid =>
            send(Msg(nick, factoid))
          }
        case _ =>
      }


    case _ =>
  }

  def lookup(name: String): Option[String] = {
    db.withSession { implicit s =>
      factoidByToken(name).map(_.description).firstOption
    }
  }

  def define(from: Nick, name: String, description: String): Unit = {
    db.withSession { implicit s =>
      factoidByToken(name).firstOption match {
        case Some(f) =>
          val newF = f.copy(description = description, dateLastEdit = new DateTime())
          factoidByToken(name).update(newF)
        case None =>
          factoids += Factoid(name, description)
      }
    }
  }

  def undefine(name: String): Unit = {
    db.withSession { implicit s =>
      factoidByToken(name).delete
    }
  }

  def search(name: String): List[Factoid] = {
    db.withSession { implicit s =>
      factoids.filter(_.description like s"%$name%").list
    }
  }

  override val helpEntries = List(
    HelpEntry(Regular, "def",       "!def <fact> = <msg>",  "Defines/Overwrite the factoid <fact> with the new text <msg>"),
    HelpEntry(Regular, "undef",     "!undef <fact>",        "Deletes factoid <fact>"),
    HelpEntry(Guest,   "search",    "!search <desc>",       "Searches for <desc> if factoids"),
    HelpEntry(Guest,   "<fact>",    "<fact>",               "Display factoid <fact>"),
    HelpEntry(Regular, "!+<fact>",  "!+<fact>",             "Public command: display factoid <fact> (Alt: nick, !+fact)")
  )
}
