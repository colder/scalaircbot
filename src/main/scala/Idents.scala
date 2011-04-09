package ircbot

case class Ident(val value: String)

class IdentCache(ctl: Control) {
    def now = System.currentTimeMillis

    val cacheTimeout = 24*60*60

    case class CachedIdent(val ident: Ident, time: Long) {
        def this(ident: Ident) = this(ident, now)

        def expired = (now-time) > (cacheTimeout*1000)
    }

    var nicksToIdents = Map[String, CachedIdent]()

    def get(nick: String): Option[Ident] = {
        nicksToIdents.get(nick) match {
            case Some(cid) if !cid.expired =>
                Some(cid.ident)
            case _ =>
                request(nick)
        }
    }

    def request(nick: String) = {
        None
    }

    def rename(nickFrom: String, nickTo: String) = {
        nicksToIdents.get(nickFrom) match {
            case Some(id) =>
                nicksToIdents -= nickFrom
                nicksToIdents += nickTo -> id
            case None =>
        }
    }

    def leave(nick: String) = {
        nicksToIdents -= nick
    }

    def cleanup = {
        nicksToIdents = nicksToIdents.filter(!_._2.expired)
    }
}
