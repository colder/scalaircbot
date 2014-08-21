package ircbot
package modules

import akka.actor._
import scala.slick.driver.MySQLDriver.simple._
import utils._
import InnerProtocol._

import db.Helpers._
import db._

class Factoids(val db: Database,
               val ctl: ActorRef) extends Module {

  override def receive = {
    // Public
    case From(NickMask(nick), Msg(chan: Channel, msg)) =>
      if (msg.contains("!+")) {
        msg.split("[:, ]? ?!\\+", 2).toList match {
          case prefix :: fact :: Nil =>
            ifGranted(nick, Regular) {
              lookup(fact).foreach { factoid =>
                if (prefix.nonEmpty) {
                  send(Msg(chan, prefix+", "+factoid))
                } else {
                  send(Msg(chan, factoid))
                }
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
    case m =>
      super.receive(m)
  }

  def lookup(name: String): Option[String] = {
    db.withSession { implicit s =>
      val of = factoidByToken(name).firstOption
      of.foreach{ f =>
        factoidByToken(name).map(_.hits).update(f.hits+1)
      }
        
      of.map(_.description)
    }
  }

  def define(from: Nick, name: String, description: String): Unit = {
    db.withSession { implicit s =>
      factoidByToken(name).firstOption match {
        case Some(f) =>
          val newF = f.copy(description = description, dateLastEdit = now(), userLastEdit = from.name)
          factoidByToken(name).update(newF)
        case None =>
          factoids += Factoid(name, FactoidKinds.User, description, dateLastEdit = now(), userDefined = from.name, userLastEdit = from.name)
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
