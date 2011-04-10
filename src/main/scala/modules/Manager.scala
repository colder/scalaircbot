package ircbot.modules

import ircbot._
import helpers.Auth

class Manager(ctl: Control) extends Module(ctl) with Auth {
    def handleMessage(msg: Message) = {
        msg match {
            case Msg(prefix, to: Channel, msg) =>
                msg.split(" ", 3).toList match {
                    case "!masks" :: "reload" :: Nil =>
                        if (isGranted(ctl, prefix, Manager)) {
                            ctl.maskStore.reload
                            ctl.p.msg(prefix.nick, "Masks Reloaded.")
                        } else {
                            ctl.p.msg(prefix.nick, "Permission denied.")
                        }
                        false
                    case "!masks" :: Nil =>
                        if (isGranted(ctl, prefix, Manager)) {
                            listAccesses(prefix)
                        } else {
                            ctl.p.msg(prefix.nick, "Permission denied.")
                        }
                        false
                    case "!say" :: to :: msg :: Nil =>
                        if (isGranted(ctl, prefix, Manager)) {
                            if (to startsWith "#") {
                                ctl.p.msg(Channel(to), msg)
                            } else {
                                ctl.p.msg(Nick(to), msg)
                            }
                            ctl.p.msg(prefix.nick, "Message transmitted.")
                        } else {
                            ctl.p.msg(prefix.nick, "Permission denied.")
                        }
                        false
                    case "!grant" :: to :: access :: Nil =>
                        if (isGranted(ctl, prefix, Manager)) {
                            grantAccess(prefix, to, access)
                        } else {
                            ctl.p.msg(prefix.nick, "Permission denied.")
                        }
                        false
                    case "!revoke" :: to :: Nil =>
                        if (isGranted(ctl, prefix, Manager)) {
                            revokeAccess(prefix, to)
                        } else {
                            ctl.p.msg(prefix.nick, "Permission denied.")
                        }
                        false
                    case "!join" :: chan :: Nil =>
                        if (isGranted(ctl, prefix, Manager)) {
                            ctl.p.join(Channel(chan))
                            ctl.p.msg(prefix.nick, "Joined channel "+chan+".")
                        } else {
                            ctl.p.msg(prefix.nick, "Permission denied.")
                        }
                        false
                    case "!part" :: chan :: Nil =>
                        if (isGranted(ctl, prefix, Manager)) {
                            ctl.p.part(Channel(chan))
                            ctl.p.msg(prefix.nick, "Left channel "+chan+".")
                        } else {
                            ctl.p.msg(prefix.nick, "Permission denied.")
                        }

                        false
                    case "!help" :: Nil =>
                        val help = userLevel(ctl, prefix) match {
                            case Manager =>
                                "!masks, !grant <m> <a>, !revoke <m>, !join <c>, !part <c>, !def <f> = <d>, !search <f>, !undef <f>"
                            case Administrator | Normal =>
                                "!search <fact>, !def <f> = <d>, !undef <f>"
                            case Guest =>
                                "Simply msg the bot privately with a factoid handle and he will answer the factoid content"
                        }

                        ctl.p.msg(prefix.nick, help);

                        false
                    case _ =>
                        true
                }
            case _ =>
                true

        }
    }

    def revokeAccess(from: Prefix, mask: String) {
        try {
            val stmt = ctl.db.prepareStatement("DELETE FROM irc_users WHERE mask = ?", mask)

            if (stmt.executeUpdate > 0) {
                ctl.p.msg(from.nick, "Access revoked to mask '"+mask+"'")
            } else {
                ctl.p.msg(from.nick, "Mask '"+mask+"' not found")
            }

            ctl.maskStore.reload

            stmt.close
        } catch {
            case ex: Exception =>
                ctl.db.handleException(ex)
        }
    }
    def grantAccess(from: Prefix, mask: String, access: String) {
        try {
            val level = UserLevel.fromString(access).toString

            ctl.db.prepareStatement("REPLACE irc_users SET mask = ?, level = ?", mask, level).executeUpdate
            ctl.p.msg(from.nick, "Access '"+level+"' granted to mask '"+mask+"'")

            ctl.maskStore.reload

        } catch {
            case ex: Exception =>
                ctl.db.handleException(ex)
        }
    }


    def listAccesses(from: Prefix) = {
        try {
            val stmt = ctl.db.prepareStatement("SELECT mask, level FROM irc_users")
            var users: List[String] = Nil;

            for (result <- stmt.executeQuery) {
                users = "'"+result.getString("mask")+"' ("+result.getString("level")+")" :: users
                if (users.size == 3) {
                    ctl.p.msg(from.nick, users.mkString("Masks: ", ",", ""))
                    users = Nil
                }
            }

            if (users.size != 0) {
                ctl.p.msg(from.nick, users.mkString("Masks: ", ",", ""))
            }

            stmt.close
        } catch {
            case ex: Exception =>
                ctl.db.handleException(ex)
        }
    }
}
