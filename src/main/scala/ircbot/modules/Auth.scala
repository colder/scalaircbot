package ircbot
package modules

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import utils._
import InnerProtocol._

import scala.collection.mutable.Map

class Auth(val ctl: ActorRef) extends Module {
  val users = new CachedMap[Nick, UserLevel](30.minutes)
  val requests = new CachedMap[Nick, Set[ActorRef]](20.seconds)

  def receive = {
    case ReceivedMessage(imsg) => imsg match {
      case From(UserMask(Nick.NickServ, _, _), Notice(_, msg)) =>
        val NickIdent = """Information on (.+) \(account (.+)\):""".r

        msg match {
          case NickIdent(nick, ident) =>
            val n = Nick(nick.replaceAll("\\p{C}", ""))
            val i = Ident(ident.replaceAll("\\p{C}", ""))
            val lvl = Guest

            users += n -> lvl

            requests.getOrElse(n, Set()).foreach { a =>
              a ! AuthUserLevel(n, lvl)
            }

            requests -= n

          case _ =>

        }

      case From(UserMask(nick, _, _), Part(channel)) =>
        users -= nick
        requests -= nick

      case From(UserMask(nick, _, _), Quit(_)) =>
        users -= nick
        requests -= nick

      case From(UserMask(nick, _, _), NickChange(newnick)) =>
        if (users contains nick) {
          users += newnick -> users(nick)
          users -= nick
        }

        if (requests contains nick) {
          requests += newnick -> requests(nick)
          requests -= nick
        }

      case _ =>
    }

    case AuthGetUserLevel(nick) =>
      if (users contains nick) {
        sender ! AuthUserLevel(nick, users(nick))
      } else {
        requests += nick -> (requests.getOrElse(nick, Set()) + sender)
        send(Msg(Nick.NickServ, "INFO "+nick.name))
      }

    case GC =>
      users.gc()
      requests.gc()

    case _ => 
  }
}
