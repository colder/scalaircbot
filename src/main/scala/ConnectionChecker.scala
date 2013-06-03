package ircbot

import akka.actor._
import utils.Logger
import scala.concurrent.duration._

import InnerProtocol._
class ConnectionChecker(timeout: Duration) extends Actor {
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
        // Try to revive connections
        trackedConnections.foreach(_ ! WriteLine("PING :"+System.currentTimeMillis))
      } else {
        context.parent ! ReinitConnection
      }
  }
}
