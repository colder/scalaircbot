package ircbot
package modules

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import utils._
import InnerProtocol._
import scala.slick.driver.MySQLDriver.simple._
import db.Helpers

class Auth(val db: Database,
           val ctl: ActorRef) extends Module {
  val users = new CachedMap[Nick, User](30.minutes)
  val requests = new CachedMap[Nick, Set[ActorRef]](20.seconds)

  def receive = {
    case ReceivedMessage(imsg) => imsg match {
      case From(NickMask(Nick.NickServ), Notice(_, msg)) =>
        val NickIdent = """Information on (.+) \(account (.+)\):""".r

        msg match {
          case NickIdent(nick, ident) =>
            val n = Nick(nick.replaceAll("\\p{C}", ""))
            val i = Ident(ident.replaceAll("\\p{C}", ""))

            val lvl = db.withSession { implicit s =>
              Helpers.users.filter(_.account === i.value).map(_.userLevel).firstOption.getOrElse(Guest)
            }

            val u = User(n, lvl)
            users += n -> u

            requests.getOrElse(n, Set()).foreach { a =>
              a ! u
            }

            requests -= n

          case _ =>

        }

      case From(NickMask(nick), Part(channel)) =>
        users -= nick
        requests -= nick

      case From(NickMask(nick), Quit(_)) =>
        users -= nick
        requests -= nick

      case From(NickMask(nick), NickChange(newnick)) =>
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

    case AuthGetUser(nick) =>
      if (users contains nick) {
        sender ! users(nick)
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
