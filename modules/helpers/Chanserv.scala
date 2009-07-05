package ircbot.modules.helpers

import scala.collection.mutable.HashMap

trait Chanserv {
    var isOp = new HashMap[String, Boolean]();

    def op(ctl: Control, channel: String) = isOp.get(channel) match {
        case Some(false) | None =>
            ctl.p.msg("chanserv", "OP "+channel)
            Thread.sleep(1000)
            isOp += channel -> true
        case _ =>
    }

    def deop(ctl: Control, channel: String) = isOp.get(channel) match {
        case Some(true) =>
            ctl.p.deop(channel, ctl.nick)
            isOp += channel -> false
        case _ =>
    }
}
