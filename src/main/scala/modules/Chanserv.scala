package ircbot
package modules

import helpers.{Auth}

import utils._

abstract class Action {
    def execute
}
case class GenericAction(body: () => Unit) extends Action {
    def execute = body()
}

case class BanAction(p: Prefix, body: () => Unit) extends Action {
    def execute = body()
}

case class DelayedAction(time: Long, requireOP: Boolean, action: Action);

class Chanserv(ctl: Control) extends Module(ctl) with Auth with Commands {
    var isOP           = Map[Channel, Boolean]().withDefaultValue(false)
    var isRequestingOP = Map[Channel, Boolean]().withDefaultValue(false)
    var delayedActions = Map[Channel, Set[DelayedAction]]().withDefaultValue(Set())

    def handleMessage(msg: Message) = {
        var passThrough = true
        msg match {
            case Mode(prefix, channel, modes, user) if user == ctl.cfg.authNick =>
                if (modes contains "+o") {
                    isOP           += (channel -> true)
                    isRequestingOP -= channel
                } else if (modes contains "-o") {
                    isOP           -= channel
                }
            case Msg(from, to, msg) =>
                words(msg, 3) match {
                    case "!unban" :: channel :: nick :: Nil =>
                        if (isGranted(ctl, from, Manager, Administrator)) {
                            if (unban(Channel(channel), Nick(nick))) {
                                ctl.p.msg(from.nick, "Nick "+nick+" unbanned form channel "+channel+".")
                            } else {
                                ctl.p.msg(from.nick, "Nick "+nick+" not currently banned in channel "+channel+".")
                            }
                        } else {
                            ctl.p.msg(from.nick, "Permission denied.")
                        }
                        passThrough = false
                    case _ =>
                }
            case _ =>
        }

        checkActionsList()

        passThrough
    }

    private def now = System.currentTimeMillis/1000;

    private def afterSeconds(channel: Channel, seconds: Int, requireOP: Boolean = false)(action: => Unit) =
        registerAction(channel, seconds, requireOP, GenericAction(() => action))

    private def checkActionsList() = {
        for((channel, acts) <- delayedActions) {
            val toPerform = acts filter { a => (now >= a.time) && (a.requireOP == false || isOP(channel))}
            delayedActions += channel -> (acts -- toPerform)

            val inOrder = toPerform.toSeq.sortWith((a,b) => a.time < b.time)

            for (a <- inOrder) {
                a.action.execute;
            }

            if (isOP(channel)) {
                if (!delayedActions(channel).exists(_.requireOP)) {
                    ctl.p.deop(channel, ctl.nick);
                }
            }
        }
    }

    private def registerAction(channel: Channel, seconds: Int, requireOP: Boolean, action: Action) =
        delayedActions += channel -> (delayedActions(channel) + DelayedAction(now + seconds, requireOP, action))

    private def registerBan(channel: Channel, p: Prefix, seconds: Int, mute: Boolean): Boolean = {
        // check that a ban is not already set
        val ob = delayedActions(channel).find{ case da @ DelayedAction(_, _, BanAction(bp, _)) => bp == p }

        if (ob.isEmpty) {
            if (mute) {
                registerAction(channel, 0,       true, BanAction(p, () => ctl.p.mute(channel, p.nickMask)))
                registerAction(channel, seconds, true, BanAction(p, () => ctl.p.unmute(channel, p.nickMask)))
            } else {
                registerAction(channel, 0,       true, BanAction(p, () => ctl.p.ban(channel, p.nickMask)))
                registerAction(channel, seconds, true, BanAction(p, () => ctl.p.unban(channel, p.nickMask)))
            }
            op(channel)
            true
        } else {
            false
        }
    }

    def mute(channel: Channel, p: Prefix, duration: Duration) =
        registerBan(channel, p, duration.toSeconds, true)

    def ban(channel: Channel, p: Prefix, duration: Duration) =
        registerBan(channel, p, duration.toSeconds, false)

    // "manual" unban
    private def unban(channel: Channel, nick: Nick) = {
        val ob = delayedActions(channel).find{ case da @ DelayedAction(_, _, BanAction(p, _)) => p.nick == nick }
        ob match {
            case Some(unban) =>
                // found the unban operation, promote it to now
                delayedActions += channel -> (delayedActions(channel) - unban)
                registerAction(channel, 0, true, unban.action)
                op(channel)
                true
            case None =>
                false
        }
    }


    def afterOP(channel: Channel, after: Duration = Now)(action: => Unit) =
        afterSeconds(channel, after.toSeconds, true)(() => action)

    def op(channel: Channel) = {
        if (!isOP(channel) && !isRequestingOP(channel)) {
            isRequestingOP += channel -> true
            ctl.p.msg(Nick.ChanServ, "OP "+channel)
        }
    }

    def doAsOP(channel: Channel)(action: => Unit) = {
        afterOP(channel)(action)
        op(channel)
    }

    override def reconnect = {
        isOP           = Map[Channel, Boolean]().withDefaultValue(false)
        isRequestingOP = Map[Channel, Boolean]().withDefaultValue(false)
    }
}
