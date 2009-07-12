package ircbot.modules

import helpers.Auth

class Protocol(ctl: Control) extends Module(ctl) with Auth {
    var registered = false
    def handleMessage(msg: Message) = msg match {
        case Notice if !registered =>
            if (!ctl.cfg.authPass.equals("")) {
                ctl.p.pass(ctl.cfg.authPass)
            }
            ctl.p.user(ctl.nick, ctl.nick, ctl.nick, ctl.cfg.authRealName)
            ctl.p.nick(ctl.nick)
            registered = true
            true
        case Invite(from, chan) =>
            if (isGranted(ctl, from, Manager)) ctl.p.join(chan)
            false
        case Ping(msg) =>
            ctl.p.pong(msg)
            false
        case Numeric(376, _) =>
            // End of MOTD, let's join channels
            for(chan <- ctl.cfg.channels)
                ctl.p.join(chan)

            true
        case _ => true
    }
}
