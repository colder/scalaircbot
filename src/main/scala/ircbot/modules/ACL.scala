package ircbot
package modules

import akka.actor._

import scala.slick.driver.MySQLDriver.simple._
import db.Helpers._
import db.User

import utils._
import InnerProtocol._

class ACL(val db: Database,
          val ctl: ActorRef) extends Module {

  override def receive = {
    // Private
    case From(NickMask(nick), Msg(to: Nick, msg)) if to != nick =>
      words(msg, 3) match {
        case "!grant" :: account :: level :: Nil =>
          requireGranted(nick, Administrator) {
            extractLevel(level) match {
              case Some(newLevel) =>
                db.withSession { implicit s =>
                  val uq = users.filter(_.account === account)
                  uq.firstOption match {
                    case Some(u) =>
                      uq.update(u.copy(userLevel = newLevel))
                    case None =>
                      users += User(account, newLevel)
                  }
                  send(Msg(nick, s"Granted level $newLevel to account $account"))
                }
              case None =>
                send(Msg(nick, s"Unknown level $level"))
            }
          }
        case _ => words(msg, 2) match {
          case "!revoke" :: account :: Nil =>
            requireGranted(nick, Administrator) {
              db.withSession { implicit s =>
                users.filter(_.account === account).delete
                send(Msg(nick, s"Access revoked to $account"))
              }
            }
          case "!acl" :: level :: Nil =>
            requireGranted(nick, Administrator) {
              extractLevel(level) match {
                case Some(level) =>
                  db.withSession { implicit s =>
                    val res = users.filter(_.userLevel === level).sortBy(_.account).list
                    if (res.isEmpty) {
                      send(Msg(nick, f"no result found"))
                    } else {
                      for (u <- res.take(10)) {
                        send(Msg(nick, f"${u.account}%-20s ${u.userLevel}"))
                      }
                      if (res.size > 10) {
                        send(Msg(nick, f"...${res.size-10} more"))
                      }
                    }
                  }
                case None =>
                  db.withSession { implicit s =>
                    users.filter(_.account === level).firstOption match {
                      case Some(u) =>
                        send(Msg(nick, f"${u.account}%-20s ${u.userLevel}"))
                      case None =>
                        send(Msg(nick, f"no result found"))
                    }
                  }
              }
            }
          case _ =>
        }
      }
    case _ =>
  }

  def extractLevel(lvl: String): Option[UserLevel] = lvl match {
    case "big boss" | "manager"     => Some(Manager)
    case "admin" | "administrator"  => Some(Administrator)
    case "regular"                  => Some(Regular)
    case "guest"                    => Some(Guest)
    case _                          => None
  }

  override val helpEntries = List(
    HelpEntry(Regular, "grant",      "!grant <account> <level>", "Grant level <level> to account <account>"),
    HelpEntry(Regular, "revoke",     "!revoke <user>",           "Revoke all accesses to <user>"),
    HelpEntry(Guest,   "acl",        "!acl <lvl>/<account>",     "Lists ACL for <lvl>/<account>")
  )
}
