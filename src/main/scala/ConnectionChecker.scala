package ircbot

import akka.actor._
import utils.Logger
import scala.concurrent.duration._

import InnerProtocol._
class ConnectionChecker(c: ActorRef, timeout: Duration) extends Actor {
  var tries    = 0
  var maxTries = 3

  override def preStart = {
    c ! StartListening
  }

  context.setReceiveTimeout(timeout)

  def receive = {
    case ReadLine(line) =>
      tries = 0

    case ReceiveTimeout =>
      if (tries < maxTries) {
        tries += 1
        c ! WriteLine("PING :"+System.currentTimeMillis)
      } else {
        context.parent ! ReinitConnection
      }
  }

  override def postStop = {
    c ! StopListening
  }
}
