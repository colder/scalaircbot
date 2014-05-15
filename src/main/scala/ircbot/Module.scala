package ircbot

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import InnerProtocol._
import utils._
import scala.concurrent.duration._
import scala.concurrent._
import ExecutionContext.Implicits.global
import java.util.concurrent.TimeoutException

abstract class Module extends Actor with RemoteLogger {
  val ctl: ActorRef

  implicit val tm = Timeout(10.seconds)

  def send(message: Message) {
    ctl ! SendMessage(message)
  }

  protected def getUser(nick: Nick): Future[User] = {
    ctl.ask(SendTo("auth", AuthGetUser(nick)))(10.seconds).mapTo[User].fallbackTo(Future(User.default(nick)))
  }

  def requireGranted(nick: Nick, lvl: UserLevel)(onGranted: => Any) = {
    ifGranted(nick, lvl) {
      onGranted
    } {
      send(Msg(nick, "Permission denied: this command requires at least the "+lvl+" right!"))
    }
  }

  def ifGranted[T](nick: Nick, lvl: UserLevel)(onGranted: => T)(onNotGranted: => T): Future[T] = {
    getUser(nick).map { u =>
      if (u.level >= lvl) {
        onGranted
      } else {
        onNotGranted
      }
    }
  }

  def words(str: String): List[String] = words(str, 0)

  def words(str: String, limit: Int): List[String] =
    str.split("[:,. ]", limit).toList
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
