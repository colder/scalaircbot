package ircbot

import akka.actor.ActorRef

/* Inner protocol between Control, Connection and the ConnectionChecker */
object InnerProtocol {
  object Start
  object StartListening
  object StopListening

  // Write a line to the server
  case class WriteLine(line: String);
  // Response from the server
  case class ReadLine(line: String);
  // Reinit the connection to the server
  case object ReinitConnection;
  // Reconnect to the server
  case object StopChecker;

  case class TrackConnection(c: ActorRef)
  case class UntrackConnection(c: ActorRef)
}

