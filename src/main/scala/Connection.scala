package ircbot

import akka.actor._
import akka.actor.IO._
import akka.util.ByteString

import scala.collection.mutable.Queue

import utils.Logger

import InnerProtocol._

class ConnectionClosedException(cause: String) extends Exception(cause);

class Connection(host: String, port: Int, logger: Logger) extends Actor {
  var messages: List[Long] = Nil
  val timespan  = 10
  val threshold = 20

  type Listener = ActorRef
  var listeners = Set[Listener]()


  var socket: SocketHandle    = null

  def dispatchLine(line : String) = {
    logger in line

    for (l <- listeners) {
      l ! ReadLine(line)
    }
  }

  var state: IterateeRef[Unit] = null

  val EOL = ByteString("\r\n")

  override def preStart = {
    socket = IOManager(context.system).connect(host, port)
    state = IO.IterateeRef.sync()

    def readOneLine: IO.Iteratee[Unit] =
      for(msg <- IO takeUntil EOL) yield dispatchLine(msg.utf8String)

    state flatMap (_ => IO repeat readOneLine)
  }

  def receive = {
    case IO.Read(rHandle, bytes) =>
      state(IO Chunk bytes)

    case IO.Closed(rHandle, cause) =>
      logger err cause.toString
      throw new ConnectionClosedException(cause.toString)

    case StartListening =>
      listeners += sender
    case StopListening =>
      listeners -= sender

    case WriteLine(line) =>
      addMessage()

      if (isFlooding) {
          logger.warn("Flood detected, delaying...");
          Thread.sleep(2000)
      }

      logger out line

      socket write ByteString(line)
      socket write EOL

      cleanMessages()

    case _ =>
  }

  def addMessage() {
    messages = System.currentTimeMillis :: messages
  }

  def cleanMessages() {
    if (messages.size > 50) {
      messages = messages filter { _ > System.currentTimeMillis-timespan*2*1000 }
    }
  }

  def isFlooding = {
    messages.filter{ _ > System.currentTimeMillis-timespan*1000 }.length >= threshold
  }
}
