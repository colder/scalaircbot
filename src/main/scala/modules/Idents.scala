package ircbot
package modules

import scala.actors.Actor

import InnerProtocol._
import utils.Commands

class Idents(val ctl: Control) extends Module(ctl) with NickTracker with Commands {
    def now = System.currentTimeMillis

    val cacheTimeout = 24*60*60

    case class CachedIdent(val ident: Ident, time: Long) {
        def this(ident: Ident) = this(ident, now)

        def expired = (now-time) > (cacheTimeout*1000)
    }

    private var nicksToIdents = Map[Nick, CachedIdent]()
    private var identsToAuth  = Map[Ident, UserLevel]()

    def handleMessage(msg: Message) = msg match {
        case Msg(from, to: Nick, Words2("!ident", nick)) =>
            requireAuth(from, Manager, Administrator) {
                getIdent(Nick(nick)) match {
                    case Some(id: Ident) =>
                        ctl.p.msg(from.nick, "Ident for "+nick+" is: "+id.value+" (Access is: "+getIdentAuth(id)+")")
                    case None =>
                        ctl.p.msg(from.nick, "Ident for "+nick+" is: <none>")
                }
            }
            false
        case _ =>
            true
    }

    def getAuth(nick: Nick): UserLevel = {
        getIdent(nick) match {
            case Some(id) => getIdentAuth(id)
            case None => Guest
        }
    }

    def getIdentAuth(ident: Ident): UserLevel = {
        identsToAuth.get(ident) match {
            case Some(ul) => ul
            case None =>
                loadIdentAuth(ident)

                identsToAuth.getOrElse(ident, Guest)
        }
    }

    private def loadIdentAuth(ident: Ident) = {
        try {
            val stmt = ctl.db.prepareStatement("SELECT account, level FROM irc_users WHERE account=?", ident.value)
            val results = stmt.executeQuery

            results.foreach { row =>
                val level = row.getString("level") match {
                    case "regular" => Regular
                    case "administrator" => Administrator
                    case "manager" => Manager
                    case _ => Guest
                }

                identsToAuth += ident -> level
            }

            stmt.close
        } catch {
            case ex: Exception =>
                ctl.db.handleException(ex)
        }

    }

    def clearIdentAuthCache() = {
        identsToAuth = Map()
    }

    def getIdent(nick: Nick): Option[Ident] = {
        nicksToIdents.get(nick) match {
            case Some(cid) if !cid.expired =>
                Some(cid.ident)
            case _ =>
                request(nick) match {
                    case Some(id) =>
                        nicksToIdents += nick -> new CachedIdent(id)
                        Some(id)
                    case None =>
                        None
                }
        }
    }

    private def request(nick: Nick) = {
        execute {
            ctl.p.msg(Nick.NickServ, "INFO "+nick.name)
        } onReply {
            case Notice(Prefix(Nick.NickServ, _, _), msg) if (msg startsWith "Information on") && (msg.toLowerCase contains nick.name.toLowerCase) =>
                msg.split("\\(account ").toList match {
                    case _ :: acc :: Nil =>
                        Some(Ident(acc.substring(0, acc.length-2).trim))
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
