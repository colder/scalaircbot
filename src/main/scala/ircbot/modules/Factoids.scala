package ircbot
package modules

import akka.actor._
import slick.driver.MySQLDriver.api._
import utils._
import InnerProtocol._
import scala.concurrent.Future

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
              for (of <- lookup(fact); f <- of) {
                if (prefix.nonEmpty) {
                  send(Msg(chan, prefix+", "+f))
                } else {
                  send(Msg(chan, f))
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
            for (results <- search(fact)) {
              results match {
                case fact :: Nil =>
                  send(Msg(nick, "Found 1 result: "+fact.token+" := "+fact.description))

                case Nil =>
                  send(Msg(nick, "Found 0 result"))

                case facts =>
                  send(Msg(nick, "Found "+facts.size+" results: "+facts.map(_.token).take(10).mkString(", ")+(if(facts.size > 10) "..." else "")))
              }
            }
          }
        case _ if !msg.startsWith("!") =>
          for (of <- lookup(msg)) {
            val answer = of.getOrElse("?")
            send(Msg(nick, answer))
          }
        case _ =>
      }
    case m =>
      super.receive(m)
  }

  def lookup(name: String): Future[Option[String]] = {
    // Increment hits
    val qhits = factoidByToken(name).map(_.hits)
    for (oh <- db.run(qhits.result.headOption); h <- oh) {
      db.run(qhits.update(h+1))
    }

    // return description
    db.run(factoidByToken(name).map(_.description).result.headOption)
  }

  def define(from: Nick, name: String, description: String): Unit = {
    for (of <- db.run(factoidByToken(name).result.headOption)) {
      of match {
        case Some(f) =>
          val q = factoidByToken(name).map(f => (f.description, f.dateLastEdit, f.userLastEdit))
          db.run(q.update((description, now(), from.name)))

        case None =>
          db.run(factoids += Factoid(name, FactoidKinds.User, description, now(), 0, from.name, from.name))
      }

    }
  }

  def undefine(name: String): Unit = {
    db.run(factoidByToken(name).delete)
  }

  def search(name: String): Future[Seq[Factoid]] = {
    db.run(factoids.filter(_.description like s"%$name%").result)
  }

  override val helpEntries = List(
    HelpEntry(Regular, "def",       "!def <fact> = <msg>",  "Defines/Overwrite the factoid <fact> with the new text <msg>"),
    HelpEntry(Regular, "undef",     "!undef <fact>",        "Deletes factoid <fact>"),
    HelpEntry(Guest,   "search",    "!search <desc>",       "Searches for <desc> if factoids"),
    HelpEntry(Guest,   "<fact>",    "<fact>",               "Display factoid <fact>"),
    HelpEntry(Regular, "!+<fact>",  "!+<fact>",             "Public command: display factoid <fact> (Alt: nick, !+fact)")
  )
}
