package ircbot

import akka.actor._
import utils.Logger
import scala.concurrent.duration._

import InnerProtocol._
class ConnectionChecker(timeout: Duration, l: Logger) extends Actor {
  var tries    = 0
  var maxTries = 3

  var trackedConnections = Set[ActorRef]()

  context.setReceiveTimeout(timeout)

  def receive = {
    case TrackConnection(c: ActorRef) =>
      trackedConnections += c
      c ! StartListening

    case UntrackConnection(c: ActorRef) =>
      trackedConnections += c
      c ! StopListening

    case ReadLine(line) =>
      tries = 0

    case ReceiveTimeout =>
      if (tries < maxTries) {
        tries += 1
        l info "[checker] Trying to revive connection ("+tries+"/"+maxTries+")..."
        trackedConnections.foreach(_ ! WriteLine("PING :"+System.currentTimeMillis))
      } else {
        tries = 0
        context.parent ! ReinitConnection
      }
  }
}
