package ircbot
package modules

import akka.actor._

import utils._
import InnerProtocol._

class Help(val ctl: ActorRef) extends Module {

  var allEntries = Seq[HelpEntry]()

  override def receive = {
    case HelpEntries(es) =>
      allEntries ++= es

    case From(NickMask(nick), Msg(chan: Nick, msg)) =>
      words(msg, 2) match {
        case "!help" :: Nil =>
          getEntriesFor(nick) map displayEntries(nick)
        case "!help" :: key :: Nil =>
          getEntriesFor(nick).map(_.filter(_.name == key)) map displayEntries(nick)
        case _ =>
      }

    case m =>
      super.receive(m)
  }

  def getEntriesFor(nick: Nick) = {
    getUser(nick).map {
      case Some(u) =>
        allEntries.filter(_.minLevel <= u.userLevel)
      case None =>
        Nil
    }
  }

  def displayEntries(nick: Nick)(es: Seq[HelpEntry]): Unit = {
    if (es.isEmpty) {
      send(Msg(nick, "No help found"))
    } else {
      for (e <- es) {
        send(Msg(nick, f"${e.command}%-30s ${e.description}"))
      }
    }
  }

  override val helpEntries = List(
    HelpEntry(Guest, "help",      "!help",                "This help")
  )
}
