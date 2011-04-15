package ircbot
package modules

import utils.Commands

class Manager(val ctl: Control) extends Module(ctl) with Commands {
    def handleMessage(msg: Message) = {
        msg match {
            case Msg(prefix, to: Nick, msg) =>
                msg.split(" ", 3).toList match {
                    case "!idents" :: "clear" :: Nil =>
                        requireAuth(prefix, Manager) {
                            ctl.idents.clearIdentAuthCache
                            ctl.p.msg(prefix.nick, "Idents cache cleared.")
                        }
                        false
                    case "!idents" :: Nil =>
                        requireAuth(prefix, Manager) {
                            listAccesses(prefix)
                        }
                        false
                    case "!say" :: to :: msg :: Nil =>
                        requireAuth(prefix, Manager) {
                            if (to startsWith "#") {
                                ctl.p.msg(Channel(to), msg)
                            } else {
                                ctl.p.msg(Nick(to), msg)
                            }
                            ctl.p.msg(prefix.nick, "Message transmitted.")
                        }
                        false
                    case "!grant" :: to :: access :: Nil =>
                        requireAuth(prefix, Manager) {
                            grantAccess(prefix, to, access)
                        }
                        false
                    case "!revoke" :: to :: Nil =>
                        requireAuth(prefix, Manager) {
                            ctl.p.msg(prefix.nick, "Permission denied.")
                        }
                        false
                    case "!join" :: chan :: Nil =>
                        requireAuth(prefix, Manager) {
                            ctl.p.join(Channel(chan))
                            ctl.p.msg(prefix.nick, "Joined channel "+chan+".")
                        }
                        false
                    case "!part" :: chan :: Nil =>
                        requireAuth(prefix, Manager) {
                            ctl.p.part(Channel(chan))
                            ctl.p.msg(prefix.nick, "Left channel "+chan+".")
                        }

                        false
                    case "!help" :: Nil =>
                        val help = ctl.idents.getAuth(prefix.nick) match {
                            case Manager =>
                                "!idents, !grant <account> <access>, !revoke <account>, !join <c>, !part <c>, !def <f> = <d>, !search <f>, !undef <f>"
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

    def revokeAccess(from: Prefix, account: String) {
        try {
            val stmt = ctl.db.prepareStatement("DELETE FROM irc_users WHERE account = ?", account)

            if (stmt.executeUpdate > 0) {
                ctl.p.msg(from.nick, "Access revoked to account '"+account+"'")
            } else {
                ctl.p.msg(from.nick, "Account '"+account+"' not found")
            }

            ctl.idents.clearIdentAuthCache

            stmt.close
        } catch {
            case ex: Exception =>
                ctl.db.handleException(ex)
        }
    }
    def grantAccess(from: Prefix, account: String, access: String) {
        try {
            val level = UserLevel.fromString(access).toString

            ctl.db.prepareStatement("REPLACE irc_users SET account = ?, level = ?", account, level).executeUpdate
            ctl.p.msg(from.nick, "Access '"+level+"' granted to account '"+account+"'")

            ctl.idents.clearIdentAuthCache

        } catch {
            case ex: Exception =>
                ctl.db.handleException(ex)
        }
    }


    def listAccesses(from: Prefix) = {
        try {
            val stmt = ctl.db.prepareStatement("SELECT account, level FROM irc_users")
            var users: List[String] = Nil;

            for (result <- stmt.executeQuery) {
                users = "'"+result.getString("account")+"' ("+result.getString("level")+")" :: users
                if (users.size == 3) {
                    ctl.p.msg(from.nick, users.mkString("Accounts: ", ",", ""))
                    users = Nil
                }
            }

            if (users.size != 0) {
                ctl.p.msg(from.nick, users.mkString("Accounts: ", ",", ""))
            }

            stmt.close
        } catch {
            case ex: Exception =>
                ctl.db.handleException(ex)
        }
    }
}
