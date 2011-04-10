package ircbot
package utils

import modules.NickTracker

case class Ident(val value: String)

class Idents(ctl: Control) extends NickTracker {
    def now = System.currentTimeMillis

    val cacheTimeout = 24*60*60

    case class CachedIdent(val ident: Ident, time: Long) {
        def this(ident: Ident) = this(ident, now)

        def expired = (now-time) > (cacheTimeout*1000)
    }

    var nicksToIdents = Map[Nick, CachedIdent]()

    def get(nick: Nick): Option[Ident] = {
        nicksToIdents.get(nick) match {
            case Some(cid) if !cid.expired =>
                Some(cid.ident)
            case _ =>
                request(nick)
        }
    }

    def request(nick: Nick) = {
        None
    }

    def rename(nickFrom: Nick, nickTo: Nick) = {
        nicksToIdents.get(nickFrom) match {
            case Some(id) =>
                nicksToIdents -= nickFrom
                nicksToIdents += nickTo -> id
            case None =>
        }
    }

    def leave(nick: Nick) = {
        nicksToIdents -= nick
    }

    def cleanup = {
        nicksToIdents = nicksToIdents.filter(!_._2.expired)
    }

    def userJoins(p: Prefix, channel: Channel) = () /* ignore */
    def userParts(p: Prefix, channel: Channel) = leave(p.nick)
    def userQuits(p: Prefix) = leave(p.nick)
    def userRenames(p: Prefix, newnick: Nick) = rename(p.nick, newnick)
}
