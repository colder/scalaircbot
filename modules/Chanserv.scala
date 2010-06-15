package ircbot.modules

import ircbot._
import helpers.Auth


case class DelayedAction(time: Long, requireOP: Boolean = false, action: () => Unit);
class Chanserv(ctl: Control) extends Module(ctl) with Auth {
    var opStatus       = Map[String, Boolean]().withDefaultValue(false)
    var delayedActions = Map[String, Set[DelayedAction]]().withDefaultValue(Set())


    def handleMessage(msg: Message) = {
        msg match {
            case Mode(prefix, channel, modes, user) if user == ctl.cfg.authNick =>
                if (modes contains "+o") {
                    opStatus += (channel -> true)
                } else if (modes contains "-o") {
                    opStatus -= channel
                }
            case _ =>

        }

        checkActionsList
        true
    }

    def now = System.currentTimeMillis/1000;

    def afterSeconds(channel: String, seconds: Int, requireOP: Boolean = false)(action: => Unit) =
        registerAction(channel, seconds, requireOP, () => action)

    def checkActionsList = {
        for((channel, acts) <- delayedActions) {
            val toPerform = acts filter { a => (now >= a.time) && (a.requireOP == false || isOP(channel))}
            delayedActions += channel -> (acts -- toPerform)

            val inOrder = toPerform.toSeq.sortWith((a,b) => a.time < b.time)

            for (a <- inOrder) {
                a.action();
            }

            if (isOP(channel)) {
                if (!delayedActions(channel).exists(_.requireOP)) {
                    ctl.p.deop(channel, ctl.nick);
                }
            }
        }
    }

    def registerAction(channel: String, seconds: Int, requireOP: Boolean, action: () => Unit) =
        delayedActions += channel -> (delayedActions(channel) + DelayedAction(now + seconds, requireOP, () => action))


    def afterOP(channel: String)(action: => Unit) =
        afterSeconds(channel, 0, true)(() => action)

    def isOP(channel: String) = opStatus(channel)

    def op(channel: String) = {
        if (!isOP(channel)) {
            ctl.p.msg("chanserv", "OP "+channel)
        }
    }

    def doAsOP(channel: String)(action: => Unit) = {
        afterOP(channel)(action)
        op(channel)
    }

}
