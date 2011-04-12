package ircbot
package modules

import helpers.Auth

import utils.Commands

class Factoids(val ctl: Control) extends Module(ctl) with Auth with Commands {
    def handleMessage(msg: Message) = msg match {
        case Msg(from, to: Channel, msg) =>
            // msg sent on channel
            if (msg.startsWith("!+")) {
                if (isGranted(ctl, from, Normal, Manager, Administrator)) {
                    sendFact(to, msg.substring(2), false)
                } else {
                    ctl.p.msg(from.nick, "This public command can only be used by regulars. You can simply msg me: /msg php-bot "+msg.substring(2))
                }
            } else if (msg.startsWith("!?")) {
                if (isGranted(ctl, from, Normal, Manager, Administrator)) {
                    searchFacts(from, to, msg.substring(2))
                } else {
                    ctl.p.msg(from.nick, "This public command can only be used by regulars.")
                }

            } else if (msg.startsWith("!tell")) {
                words(msg, 4) match {
                    case "!tell" :: nick :: "about" :: fact :: Nil =>
                        sendFact(Nick(nick), fact, false)
                    case xs =>
                }
            } else {
                msg.split("[:,] ?!\\+", 2).toList match {
                    case nick :: fact :: Nil =>
                        if (isGranted(ctl, from, Normal, Manager, Administrator)) {
                            lookup(fact) match {
                                case Some(x) => ctl.p.msg(to, nick+", "+x);
                                case None =>
                            }
                        } else {
                            ctl.p.msg(from.nick, "This public command can only be used by regulars. You can simply msg me: /msg php-bot "+fact)
                        }
                    case xs =>
                }
            }
            true
        case Msg(from, to: Nick, msg) =>
            // msg sent on private
            if (to.equals(from.nick)) {
                ctl.error("Sending message to myself..")
            } else if (!msg.equals("VERSION")){
                words(msg, 2) match {
                    case "!def" :: rest :: Nil => rest.split("=", 2).toList match {
                        case fact :: description :: Nil => 
                            if (isGranted(ctl, from, Normal, Manager, Administrator)) {
                                defineFact(from, fact.trim, description.trim)
                            } else {
                                ctl.p.msg(from.nick, "Permission denied.")
                            }
                        case _ =>
                            ctl.p.msg(from.nick, "?")
                    }
                    case "!undef" :: fact :: Nil =>
                        if (isGranted(ctl, from, Normal, Manager, Administrator)) {
                            undefineFact(from, fact)
                        } else {
                            ctl.p.msg(from.nick, "Permission denied.")
                        }
                    case "!search" :: fact :: Nil =>
                        if (isGranted(ctl, from, Normal, Manager, Administrator)) {
                            searchFacts(from, from.nick, fact)
                        } else {
                            ctl.p.msg(from.nick, "Permission denied.")
                        }
                    case _ =>
                        sendFact(from.nick, msg, true)
                }
            }
            true
        case _ => true
    }

    def sendFact(to: AbsChannel, pattern: String, sendError: Boolean) = lookup(pattern) match {
        case Some(x) => ctl.p.msg(to, x)
        case None => if (sendError) ctl.p.msg(to, "?")
    }

    def undefineFact(from: Prefix, pattern: String) {
        try {
            val stmt = ctl.db.prepareStatement("DELETE FROM irc_factoids WHERE token = ?", pattern)

            if (stmt.executeUpdate > 0) {
                ctl.p.msg(from.nick, "Factoid "+pattern+" removed")
            } else {
                ctl.p.msg(from.nick, "Factoid "+pattern+" not found")
            }

            stmt.close
        } catch {
            case ex: Exception =>
                ctl.db.handleException(ex)
        }
    }
    def defineFact(from: Prefix, pattern: String, description: String) {
        try {
            val stmt = ctl.db.prepareStatement("REPLACE irc_factoids SET description = ?, date_lastedit = NOW(), hits = 0, token = ?", description, pattern)

            if (stmt.executeUpdate > 0) {
                ctl.p.msg(from.nick, "Factoid "+pattern+" updated")
            } else {
                ctl.p.msg(from.nick, "Factoid "+pattern+" not updated")
            }

            stmt.close
        } catch {
            case ex: Exception =>
                ctl.db.handleException(ex)
        }
    }

    def searchFacts(from: Prefix, to: AbsChannel, pattern: String) = {
        try {
            val stmt = ctl.db.prepareStatement("SELECT SQL_CALC_FOUND_ROWS token, description FROM irc_factoids WHERE MATCH(token, description) AGAINST(?) LIMIT 10", pattern)
            val results = stmt.executeQuery

            val rowcount = ctl.db.prepareStatement("SELECT FOUND_ROWS() as number").executeQuery.firstRow.getInt("number")
            if (rowcount == 1) {
                val result = results.firstRow
                ctl.p.msg(to, "Found "+result.getString("token")+": "+result.getString("description"))
            } else if (rowcount == 0) {
                ctl.p.msg(to, "Nothing found")
            } else {
                var facts: List[String] = Nil;
                 for(result <- results) {
                    facts = facts ::: "'"+result.getString("token")+"'" :: Nil
                }
                ctl.p.msg(to, "Found "+rowcount+" results: "+facts.mkString(", ")+(if (rowcount > 10) "..." else ""))
            }

            stmt.close
        } catch {
            case ex: Exception =>
                ctl.db.handleException(ex)
        }
    }

    def lookup(pattern: String): Option[String] = {
        try {
            val stmt = ctl.db.prepareStatement("SELECT description FROM irc_factoids WHERE token = ?", pattern)
            val results = stmt.executeQuery

            val retval = if (results.hasNext) {
                ctl.db.prepareStatement("UPDATE irc_factoids SET hits = hits +1 WHERE token = ?", pattern).executeUpdate
                Some(results.firstRow.getString("description"))
            } else {
                None
            }

            stmt.close
            retval
        } catch {
        case ex: Exception =>
            ctl.db.handleException(ex)
            None
        }
    }
}
