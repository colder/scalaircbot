package ircbot.modules

class Protocol(ctl: Control) extends Module(ctl) {
    var registered = false
    var registerInfo: Option[(String, String, String, Option[String])] = None
    def handleMessage(msg: Message) = {
        msg match {
            case Notice if !registered =>
                registerInfo match {
                    case Some((hostname, servername, realname, password)) =>
                        password match {
                            case Some(p) =>
                                ctl.p.pass(p)
                            case None =>
                        }
                        ctl.p.user(ctl.nick, hostname, servername, realname)
                        ctl.p.nick(ctl.nick)
                        registered = true
                    case None =>
                        ctl.error("Not yet registered!");
                }
                true

            case Ping(msg) =>
                ctl.p.pong(msg)
                false
            case _ => true
        }
    }

    def scheduleRegistration(hostname: String, servername: String, realname: String, password: Option[String]) = {
        registerInfo = Some((hostname, servername, realname, password));
    }
}
