package ircbot
package modules

import utils._

class Protocol(val ctl: Control) extends Module(ctl) with Commands {
    def handleMessage(msg: Message) = msg match {
        case Msg(from, to, m) if from.nick == "freenode-connect" =>
            // ignore VERSION messages from freenode-connect
            false
        case Invite(from, chan) =>
            if (isGranted(from, Manager)) ctl.p.join(chan)
            false
        case Ping(msg) =>
	    ctl.l.info("Asking for writeline: ")
            ctl.p.pong(msg)
            false
        case _ => true
    }
}
