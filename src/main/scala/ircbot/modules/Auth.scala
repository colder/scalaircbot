package ircbot
package modules

import akka.actor._

import org.joda.time.Period

import slick.driver.MySQLDriver.api._
import db.Helpers._
import db.User

import utils._
import InnerProtocol._

class Auth(val db: Database,
           val ctl: ActorRef) extends Module {

  val cache    = new CachedMap[Nick, User](Period.minutes(30))
  val requests = new CachedMap[Nick, Set[ActorRef]](Period.seconds(30))

  override def receive = {
    case From(NickMask(Nick.NickServ), Notice(_, msg)) =>
      val NickIdent = """Information on (.+) \(account (.+)\):""".r

      msg match {
        case NickIdent(rawNick, rawAccount) =>
          val nick    = Nick(rawNick.replaceAll("[^\\p{Graph}]", ""))
          val account = rawAccount.replaceAll("[^\\p{Graph}]", "")

          val q = users.filter(_.account === account).map(_.userLevel)


          for (olvl <- db.run(q.result.headOption)) {
            val lvl = olvl.getOrElse(Guest)

            logInfo(s"Authenticated ${nick.name} (account: $account) to level: $lvl")

            val user = User(account, lvl)
            cache += nick -> user

            requests.getOrElse(nick, Set()).foreach { aref =>
              aref ! Some(user)
            }

            requests -= nick
          }

        case _ =>

      }

    case From(NickMask(nick), Part(channel)) =>
      cache -= nick
      requests -= nick

    case From(NickMask(nick), Quit(_)) =>
      cache -= nick
      requests -= nick

    case From(NickMask(nick), NickChange(newnick)) =>
      if (cache contains nick) {
        cache += newnick -> cache(nick)
        cache -= nick
      }

      if (requests contains nick) {
        requests += newnick -> requests(nick)
        requests -= nick
      }

    case AuthGetUser(nick) =>
      if (cache contains nick) {
        sender ! Some(cache(nick))
      } else {
        requests += nick -> (requests.getOrElse(nick, Set()) + sender)
        send(Msg(Nick.NickServ, "INFO "+nick.name))
      }

    case GC =>
      cache.gc()
      requests.gc()

    case m =>
      super.receive(m)
  }
}
