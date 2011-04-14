package ircbot
package modules

import scala.actors.Actor

import InnerProtocol._
import utils.Commands

case class Ident(val value: String)

class Idents(val ctl: Control) extends Module(ctl) with NickTracker with Commands {
    def now = System.currentTimeMillis

    val cacheTimeout = 24*60*60

    case class CachedIdent(val ident: Ident, time: Long) {
        def this(ident: Ident) = this(ident, now)

        def expired = (now-time) > (cacheTimeout*1000)
    }

    var nicksToIdents = Map[Nick, CachedIdent]()


    def handleMessage(msg: Message) = {
        msg match {
            case Msg(from, to: Nick, Words2("!ident", nick)) =>
                requireAuth(from, Manager, Administrator) {
                    get(Nick(nick)) match {
                        case Some(Ident(id)) =>
                            ctl.p.msg(from.nick, "Ident for "+nick+" is: "+id)
                        case None =>
                            ctl.p.msg(from.nick, "Ident for "+nick+" is: <none>")
                    }
                }
            case _ =>
        }
        true
    }

    def get(nick: Nick): Option[Ident] = {
        nicksToIdents.get(nick) match {
            case Some(cid) if !cid.expired =>
                Some(cid.ident)
            case _ =>
                request(nick)
        }
    }

    private def request(nick: Nick) = {
        execute {
            ctl.p.msg(Nick.NickServ, "INFO "+nick.name)
        } onReply {
            case Notice(Prefix(Nick.NickServ, _, _), msg) if (msg startsWith "Information on") && (msg contains nick.name) =>
                msg.split("\\(account ").toList match {
                    case _ :: acc :: Nil =>
                        Some(Ident(acc.substring(0, acc.length-2)))
                    case _ =>
                        None
                }
        }
    }


    def cleanup = {
        nicksToIdents = nicksToIdents.filter(!_._2.expired)
    }

    /* nickTracer hooks */
    def onUserJoin(p: Prefix, channel: Channel) = () /* ignore */
    def onUserPart(p: Prefix, channel: Channel) = onUserQuit(p)
    def onUserQuit(p: Prefix) = {
        nicksToIdents -= p.nick
    }
    def onUserRename(p: Prefix, newnick: Nick) = {
        nicksToIdents.get(p.nick) match {
            case Some(id) =>
                nicksToIdents -= p.nick
                nicksToIdents += newnick -> id
            case None =>
        }
    }
}
