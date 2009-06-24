package ircbot.modules

import helpers.Auth

class Channel(ctl: Control) extends Module(ctl) with Auth {
    def handleMessage(msg: Message) = {
        msg match {
            case Invite(from, chan) =>
                if (isGranted(ctl, from, Manager)) ctl.p.join(chan)
                false
            case _ =>
                true

        }
    }
}
