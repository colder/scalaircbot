package ircbot.modules

import helpers.Auth
import scala.collection.mutable.HashMap

class Chanserv(ctl: Control) extends Module(ctl) with Auth {
    var opStatus = new HashMap[String, Boolean]();
    var opActions = new HashMap[String, List[() => Unit]]()

    def handleMessage(msg: Message) = msg match {
        case Mode(prefix, channel, modes, user) if user == ctl.cfg.authNick =>
            if (modes contains "+o") {
                opStatus(channel) = true
                executeActions(channel)
                ctl.p.deop(channel, ctl.nick)
            } else if (modes contains "-o") {
                opStatus -= channel
            }
            true
        case _ =>
            true

    }

    def channelActions(channel: String) = opActions.get(channel) match {
        case Some(acts) => acts
        case None => Nil
    }

    def afterOP(channel: String)(action: => Unit) =
        opActions(channel) = ((() => action) :: channelActions(channel))

    def executeActions(channel: String) = {
        for (a <- channelActions(channel).reverse) a()
        opActions -= channel
    }

    def isOP(channel: String) = opStatus.get(channel) match {
        case Some(x) => x
        case None => false
    }

    def op(channel: String) = if (!isOP(channel) && channelActions(channel).size != 0) {
        ctl.p.msg("chanserv", "OP "+channel)
    }

    def doAsOP(channel: String)(action: => Unit) = {
        afterOP(channel)(action)
        op(channel)
    }

}
