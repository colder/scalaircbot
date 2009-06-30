package ircbot.modules

class Protocol(ctl: Control) extends Module(ctl) {
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
        case Ping(msg) =>
            ctl.p.pong(msg)
            false
        case _ => true
    }
}
