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
import db.Helpers._
import db.User

class Auth(val db: Database,
           val ctl: ActorRef) extends Module {
  val users    = new CachedMap[Nick, User](30.minutes)
  val requests = new CachedMap[Nick, Set[ActorRef]](30.seconds)

  override def receive = {
    case ReceivedMessage(imsg) => imsg match {
      case From(NickMask(Nick.NickServ), Notice(_, msg)) =>
        val NickIdent = """Information on (.+) \(account (.+)\):""".r

        msg match {
          case NickIdent(rawNick, rawAccount) =>
            val nick    = Nick(rawNick.replaceAll("\\p{C}", ""))
            val account = rawAccount.replaceAll("\\p{C}", "")

            val lvl = db.withSession { implicit s =>
              (for {
                u <- Helpers.users if u.account === account
              } yield(u.userLevel)).firstOption.getOrElse(Guest)
            }

            logInfo(s"Authenticated ${nick.name} (account: $account) to level: $lvl")

            val user = User(account, lvl)
            users += nick -> user

            requests.getOrElse(nick, Set()).foreach { aref =>
              aref ! Some(user)
            }

            requests -= nick

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

      case m =>
        super.receive(m)
    }

    case AuthGetUser(nick) =>
      if (users contains nick) {
        sender ! Some(users(nick))
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
