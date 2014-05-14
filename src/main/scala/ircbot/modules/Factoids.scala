package ircbot
package modules

import akka.actor._
import utils._
import InnerProtocol._

class Factoids(val cfg: Config,
               val ctl: ActorRef) extends SimpleModule {

  def onMessage(msg: Message) = msg match {
    // Public
    case From(NickMask(nick), Msg(chan: Channel, msg)) =>
      if (msg.contains("!+")) {
        msg.split("[:, ]? ?!\\+", 2).toList match {
          case prefix :: fact :: Nil =>
            if (isGranted(nick, Regular)) {
              lookup(fact).foreach { factoid =>
                send(Msg(chan, prefix+", "+factoid))
              }
            } else {
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
                send(Msg(nick, "Found 1 result: "+fact.name+" := "+fact.description))

              case Nil =>
                send(Msg(nick, "Found 0 result"))

              case facts =>
              send(Msg(nick, "Found "+facts.size+" results: "+facts.map(_.name).take(10).mkString(", ")+(if(facts.size > 10) "..." else "")))
            }
          }
        case _ =>
          lookup(msg).orElse(Some("?")).foreach { factoid =>
            send(Msg(nick, factoid))
          }
      }


    case _ =>
  }

  def lookup(name: String): Option[String] = {
    None
  }

  def define(from: Nick, name: String, description: String): Boolean = {
    true
  }

  def undefine(name: String): Boolean = {
    true
  }

  def search(name: String): List[Factoid] = {
    Nil
  }
}
