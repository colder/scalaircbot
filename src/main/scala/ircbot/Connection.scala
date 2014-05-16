package ircbot

import akka.actor._
import akka.io.{ IO, Tcp }
import akka.util.ByteString
import java.net.InetSocketAddress
import scala.concurrent.duration._

import scala.collection.mutable.Queue

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

  case object Ack extends Tcp.Event

  class Throttler(max: Int, in: Duration) {
    var history = List[Long]()

    def now() = System.currentTimeMillis

    def guard(b: => Unit) {
      record()

      if (isFlooding()) {
        logInfo("Flood detected, delaying..")
        Thread.sleep(2000);
      }

      b
    }

    def record() = {
      history ::= now()

      history = history.take(3*max)
    }

    def isFlooding() = {
      history.filter(_ > now()-in.toMillis).size >= max
    }
  }

  val throttler = new Throttler(5, 3.seconds)


  var inBuffer = ""

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

        case Ack => acknowledge(connection)

        case SendRawMessage(line) =>
          buffer(line)

          if (!buffering) {
            throttler.guard {
              logOut(line)
              val data = ByteString(line+"\r\n")
              connection ! Write(data, Ack)
            }
            buffering = true
          }

        case CommandFailed(w: Write) =>
          logWarning(s"[$name] write failed (O/S buffer was full)")

        case Received(data) =>
          val str = inBuffer + data.utf8String
          var lines = str.split("\r\n").toList

          if (!str.endsWith("\r\n")) {
            inBuffer = lines.last
            lines = lines.dropRight(1)
          } else {
            inBuffer = ""
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

  var buffering = false
  var storage = new Queue[String]()

  def buffer(line: String) {
    storage enqueue line
  }

  def acknowledge(connection: ActorRef) {
    storage.dequeue

    if (storage.isEmpty) {
      buffering = false
    } else {
      val line = storage.head

      throttler.guard {
        logOut(line)
        val data = ByteString(line+"\r\n")
        connection ! Write(data, Ack)
      }
    }
  }
}
