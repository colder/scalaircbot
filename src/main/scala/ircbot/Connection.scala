package ircbot

import akka.actor._
import akka.io.{ IO, Tcp }
import akka.util.ByteString
import java.net.InetSocketAddress

import utils._

import InnerProtocol._

class Connection(host: String,
                 port: Int,
                 val logger: ActorRef,
                 val ctl:    ActorRef,
                 name: String) extends Actor with RemoteLogger {

  import context.system
  import Tcp._

  val remote = new InetSocketAddress(host, port)

  logInfo(s"[$name] Connecting...")
  IO(Tcp) ! Connect(remote)

  var messages: List[Long] = Nil
  val timespan  = 10
  val threshold = 20

  def nowMs() = System.currentTimeMillis

  def addMessage() {
    messages = nowMs() :: messages
  }

  def cleanMessages() {
    if (messages.size > 50) {
      messages = messages filter { _ > nowMs()-timespan*2*1000 }
    }
  }

  def isFlooding = {
    messages.filter{ _ > nowMs()-timespan*1000 }.length >= threshold
  }

  var buffer: String = "";

  def receive = {
    case CommandFailed(_: Connect) =>
      logError(s"[$name] Error: Connection failed")
      ctl ! Disconnected
      context stop self

    case c @ Connected(remote, local) =>
      logInfo(s"[$name] Connected!")
      ctl ! Connected

      val connection = sender
      connection ! Register(self)

      context become {
        case SendMessage(msg) =>
          IrcHelpers.messageToRawCommand(msg) match {
            case Some(data) =>
              self ! SendRawMessage(data)

            case None =>
              logWarning(s"[$name] Impossible to send "+msg)
          }

        case SendRawMessage(line) =>
          addMessage()

          if (isFlooding) {
            logWarning(s"[$name] Flood detected, delaying...")
            Thread.sleep(2000)
          }

          logOut(line)

          connection ! Write(ByteString(line+"\r\n"))

          cleanMessages()

        case CommandFailed(w: Write) =>
          logWarning(s"[$name] write failed (O/S buffer was full)")

        case Received(data) =>
          val str = buffer + data.utf8String
          var lines = str.split("\r\n").toList

          if (!str.endsWith("\r\n")) {
            buffer = lines.last
            lines = lines.dropRight(1)
          } else {
            buffer = ""
          }

          lines.foreach { line =>
            logIn(line)

            ctl ! ReceivedMessage(IrcHelpers.rawCommandToMessage(line))
          }

        case _: ConnectionClosed =>
          logWarning(s"[$name] Connection closed!")
          ctl ! Disconnected
          context stop self
      }
  }

}
