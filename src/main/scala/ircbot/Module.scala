package ircbot

import akka.actor._
import akka.pattern.ask
import scala.concurrent.duration._
import scala.concurrent._

import org.joda.time.DateTime

import InnerProtocol._
import utils._
import db.User

abstract class Module extends Actor with RemoteLogger with HelpInfo {
  val ctl: ActorRef

  implicit val dispatcher = context.dispatcher

  def send(message: Message) {
    ctl ! SendMessage(message)
  }

  protected def getUser(nick: Nick): Future[Option[User]] = {
    ctl.ask(SendTo("auth", AuthGetUser(nick)))(15.seconds).mapTo[Option[User]].fallbackTo(Future(None))
  }

  def requireGranted(nick: Nick, lvl: UserLevel)(onGranted: => Any) = {
    ifGranted(nick, lvl) {
      onGranted
    } {
      send(Msg(nick, "Permission denied: this command requires at least the "+lvl+" right!"))
    }
  }

  def ifGranted[T](nick: Nick, lvl: UserLevel)(onGranted: => T)(onNotGranted: => T): Future[T] = {
    getUser(nick).map {
      case Some(u) =>
        if (u.userLevel >= lvl) {
          onGranted
        } else {
          onNotGranted
        }
      case None =>
        send(Msg(nick, "Did not receive account information from NickServ in time :("))
        onNotGranted
    }
  }

  def currentState: Future[BotState] = {
    ctl.ask(SendTo("protocol", RequestBotState))(2.seconds).mapTo[BotState]
  }

  def requireOP(chan: Channel)(whenOp: => Any) = {
    ctl.ask(SendTo("op", RequestOp(chan)))(10.seconds).foreach { r =>
      whenOp
    }
  }

  def words(str: String): List[String] = words(str, 0)

  def words(str: String, limit: Int): List[String] =
    str.split("[:,. ]", limit).toList

  def now() = new DateTime()

  def receive = {
    case Init =>
      ctl ! SendTo("help", HelpEntries(helpEntries))

    case _ =>
  }
}
