package ircbot
package modules

import akka.actor._
import utils._
import InnerProtocol._
import org.joda.time.{DateTime, Period}

class ConnectionChecker(val ctl: ActorRef) extends Module {
  abstract class Status;

  var active = false
  var lastMessage = now()

  override def receive = {
    case Connected =>
      active = true

    case Disconnected =>
      active = false

    case ReceivedMessage(_) =>
      lastMessage = now()

    case Tick =>
      if (active) {
        if (now().isAfter(lastMessage.plus(Period.minutes(4)))) {
          ctl ! Reconnect
        } else if (now().isAfter(lastMessage.plus(Period.minutes(3)))) {
          send(Ping(now().getMillis.toString))
        }
      }

    case _ =>
      super.receive
  }

  def now() = new DateTime()
}
