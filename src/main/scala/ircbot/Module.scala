package ircbot

import akka.actor._
import InnerProtocol._
import utils._

abstract class Module(logger: ActorRef, ctl: ActorRef) extends Actor with RemoteLogger {
  def receive = {
    case ReceivedMessage(msg) =>
      onMessage(msg)

    case Disconnected =>
      onDisconnect()

    case Connected =>
      onConnect()

    case _ =>
  }

  def onMessage(message: Message): Unit

  def onDisconnect(): Unit = {}
  def onConnect(): Unit = {}
}
