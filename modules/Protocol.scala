package ircbot.modules

import helpers.Auth

class Protocol(ctl: Control) extends Module(ctl) with Auth {
    def handleMessage(msg: Message) = msg match {
        case Msg(from, to, m) if from.nick == "freenode-connect" =>
            // ignore VERSION messages from freenode-connect
            false
        case Invite(from, chan) =>
            if (isGranted(ctl, from, Manager)) ctl.p.join(chan)
            false
        case Ping(msg) =>
            ctl.p.pong(msg)
            false
        case _ => true
    }
}
