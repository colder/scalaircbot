package ircbot.modules

import helpers.Auth

class Chanserv(ctl: Control) extends Module(ctl) with Auth {
    var isOP = false;
    var opActions: List[() => Unit] = Nil

    def handleMessage(msg: Message) = msg match {
        case Mode(prefix, channel, modes, user) if user == ctl.cfg.authNick =>
            if (modes contains "+o") {
                isOP = true;
                executeActions
                ctl.p.deop(channel, ctl.nick)
            } else if (modes contains "-o") {
                isOP = false;
            }
            true
        case _ =>
            true

    }

    def afterOP(action: => Unit) = opActions = (() => action) :: opActions

    def executeActions = {
        for (a <- opActions.reverse) a()
        opActions = Nil
    }

    def op(ctl: Control, channel: String) =
        if (!isOP && opActions.size != 0) ctl.p.msg("chanserv", "OP "+channel)
}
