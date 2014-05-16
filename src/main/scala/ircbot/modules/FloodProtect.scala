package ircbot
package modules

import akka.actor._
import org.joda.time.{DateTime, Duration, PeriodType, Period}
import org.joda.time.format._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import utils._
import InnerProtocol._

import db.BanTypes._
import db.Helpers._
import db.User
import db.{BanLog => DBBanLog, BanLogs => DBBanLogs}

class FloodProtect(val ctl: ActorRef,
                   val chan: Channel) extends Module {

  val policyMax = 4
  val policyIn  = 2

  val store = new CachedMap[Nick, List[Long]](30.minutes)

  override def receive = {
    case ReceivedMessage(From(NickMask(nick), Msg(`chan`, _))) =>
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
    store.getOrElse(nick, Nil).filter(_ > (now()-policyIn*1000)).size > policyMax
  }

  def now() = System.currentTimeMillis
}
