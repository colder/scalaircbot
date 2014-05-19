package ircbot
package modules

import akka.actor._
import org.joda.time.{DateTime, Duration, Period}

import utils._
import InnerProtocol._

import db.BanTypes._

class FloodProtect(val ctl: ActorRef,
                   val chan: Channel) extends Module {

  val policyMax = 4
  val policyIn  = Period.seconds(2)

  val store = new CachedMap[Nick, List[DateTime]](Period.minutes(10))

  override def receive = {
    case From(NickMask(nick), Msg(`chan`, _)) =>
      store += nick -> (now() :: store.getOrElse(nick, Nil))

      if (isFlooding(nick)) {
        send(Msg(chan, "Muting "+nick.name+" for 20 seconds to prevent them from flooding"))
        ctl ! SendTo("banlog", RequestBan(Mute, nick, Duration.standardSeconds(20), "Automatic: flooding"))
        store -= nick
      }

    case GC =>
      store.gc()

    case msg =>
      super.receive(msg)
  }

  def isFlooding(nick: Nick): Boolean = {
    val entries = store.getOrElse(nick, Nil)

    entries.filter(_.isAfter(now().minus(policyIn))).size > policyMax
  }
}
