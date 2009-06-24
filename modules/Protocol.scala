package ircbot.modules

class Protocol(ctl: Control) extends Module(ctl) {
    var registered = false
    var registerInfo: Option[(String, String, String, String, Option[String])] = None
    def handleMessage(msg: Message) = {
        msg match {
            case Notice if !registered =>
                registerInfo match {
                    case Some((username, hostname, servername, realname, password)) =>
                        password match {
                            case Some(p) =>
                                ctl.p.pass(p)
                            case None =>
                        }
                        ctl.p.user(username, hostname, servername, realname)
                        ctl.p.nick(username)
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

    def scheduleRegistration(username: String, hostname: String, servername: String, realname: String, password: Option[String]) = {
        registerInfo = Some((username, hostname, servername, realname, password));
    }
}
