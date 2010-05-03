package ircbot.modules

import ircbot._
import helpers.Auth

class Chanserv(ctl: Control) extends Module(ctl) with Auth {
    var opStatus  = Map[String, Boolean]().withDefaultValue(false)
    var opActions = Map[String, List[() => Unit]]().withDefaultValue(Nil)
    var delayedActions   = Map[String, List[(Long, () => Unit)]]().withDefaultValue(Nil)

    def now = System.currentTimeMillis/1000;

    def handleMessage(msg: Message) = {
        msg match {
            case Mode(prefix, channel, modes, user) if user == ctl.cfg.authNick =>
                if (modes contains "+o") {
                    opStatus += (channel -> true)

                    executeActions(channel)

                    afterSeconds(channel, 6*60) {
                        if(isOP(channel)) {
                            ctl.p.deop(channel, ctl.nick)
                        }
                    }
                } else if (modes contains "-o") {
                    opStatus -= channel
                }
            case _ =>

        }

        checkActionsList
        true
    }

    def afterSeconds(channel: String, seconds: Int)(action: => Unit) =
        delayedActions += (channel -> ((now + seconds, () => action) :: delayedActions(channel)))

    def checkActionsList = {
        for((channel, acts) <- delayedActions) {
            val toPerform = acts. filter{ x => now >= x._1 }.toList
            delayedActions += (channel -> (acts filter { x => now < x._1 } toList))

            for ((time, a) <- toPerform.reverse) {
                a();
            }

        }
    }


    def afterOP(channel: String)(action: => Unit) =
        if (isOP(channel)) {
            action
        } else {
            opActions += (channel -> ((() => action) :: opActions(channel)))
        }

    def executeActions(channel: String) = {
        for (a <- opActions(channel).reverse) a()
        opActions = opActions - channel
    }

    def isOP(channel: String) = opStatus(channel)

    def op(channel: String) = if (!isOP(channel) && opActions(channel).size != 0) {
        ctl.p.msg("chanserv", "OP "+channel)
    }

    def doAsOP(channel: String)(action: => Unit) = {
        afterOP(channel)(action)
        op(channel)
    }

}
