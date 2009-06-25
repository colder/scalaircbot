package ircbot.modules

import scala.collection.mutable.HashMap

import helpers.Auth
import helpers.Commands

class MonitorHashPHP(ctl: Control) extends Module(ctl) with Auth with Commands {
    val channel = "##php"
    val timespan = 4
    val threshold = 5

    val messages = new HashMap[String, List[Long]]()

    var lastCleanup = 0

    def handleMessage(msg: Message) = msg match {
        case Msg(from, to, msg) =>
            checkMuteList

            if (to equals channel) {
                // checks that no nick sends more than n msg per m sec
                addMessage(from.nick)

                if (!isGranted(ctl, from, Normal, Manager, Administrator) && isFlooding(from.nick)) {
                    ctl.p.msg("ekneuss", "KICKING "+from.nick+"!")
                    mute(from, 300)
                    messages -= from.nick
                }

                cleanupMessages
                true
            } else {
                words(msg, 2) match {
                    case "!unban" :: mask :: Nil =>
                        if (isGranted(ctl, from, Manager, Administrator)) {
                            op

                            var n = 0;
                            for (mute <- muteList) {
                                if (mute._1 matches mask) {
                                    unmute(mute._1)
                                    ctl.p.msg(from.nick, mute._1.fullMask+" unbanned.")
                                    n += 1
                                }
                            }

                            if (n == 0) ctl.p.msg(from.nick, "Mask '"+mask+"' not found.")

                            deop
                        } else {
                            ctl.p.msg(from.nick, "Permission denied.")
                        }
                    case _ =>
                }

                false
            }
        case _ => true
    }

    def addMessage(nick: String) = messages += nick -> (messages get nick match {
        case Some(msgs) =>  System.currentTimeMillis :: msgs
        case None => System.currentTimeMillis :: Nil
    })

    def isFlooding(nick: String) = messages get nick match {
        case Some(msgs) => msgs.filter{ _ > System.currentTimeMillis-timespan*1000 }.length >= threshold
        case None => false
    }

    def cleanupMessages = {
        if (lastCleanup > 100) {
            for ( entry <- messages) entry match {
                case (nick, msgs) =>
                    val newMsgs = msgs filter { _ < System.currentTimeMillis-timespan*1000 }
                    if (newMsgs.length == 0) {
                        messages -= nick
                    } else {
                        messages += nick -> newMsgs
                    }
            }

            lastCleanup = 0
        } else {
            lastCleanup += 1
        }
    }

    val muteList = new HashMap[Prefix, (Long, Long)]();

    var isOp = false

    def op = if (!isOp) {
        ctl.p.msg("chanserv", "OP "+channel)
        Thread.sleep(1000)
        isOp = true
    }

    def deop = if (isOp) {
        ctl.p.deop(channel, ctl.nick)
        isOp = false
    }

    def mute(prefix: Prefix, duration: Long) = {
        op

        if (!(muteList contains prefix)) {
            ctl.p.mute(channel, prefix.nickMask)
        }

        muteList += prefix -> (System.currentTimeMillis/1000, duration)

        deop

    }

    def checkMuteList = {
        val toRemove = muteList filter { x => (System.currentTimeMillis/1000)-x._2._2 > x._2._1 } toList;

        if (toRemove.length > 0) {
            op

            for (mute <- toRemove ) {
                unmute(mute._1)
            }

            deop
        }
    }

    def unmute(prefix: Prefix) = {
        ctl.p.unmute(channel, prefix.nickMask)
        muteList -= prefix
    }
}
