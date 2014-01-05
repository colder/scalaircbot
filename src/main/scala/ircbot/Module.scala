package ircbot

import akka.actor._
import akka.pattern.ask
import InnerProtocol._
import utils._
import scala.concurrent.duration._
import scala.concurrent.Await
import java.util.concurrent.TimeoutException

abstract class Module extends Actor with RemoteLogger {
  val ctl: ActorRef

  def send(message: Message) {
    ctl ! SendMessage(message)
  }

  def isGranted(nick: Nick, lvl: UserLevel): Boolean = {
    val f = ctl.ask(SendTo("auth", AuthGetUserLevel(nick)))(10.seconds).mapTo[AuthUserLevel]

    val lvl = try { 
      Await.result(f, 10.seconds).lvl
    } catch {
      case e: TimeoutException =>
        Guest
    }

    lvl.hierarchy >= lvl.hierarchy
  }
}

abstract class SimpleModule extends Module {
  def receive = {
    case ReceivedMessage(msg) =>
      onMessage(msg)

    case Connected =>
      onConnect()

    case Disconnected =>
      onDisconnect()

    case _ =>
  }

  def onMessage(message: Message): Unit

  def onDisconnect(): Unit = {}
  def onConnect(): Unit = {}
}
