package ircbot


abstract class UserLevel {
    override def toString = this match {
        case Guest => "guest"
        case Normal => "normal"
        case Administrator => "administrator"
        case Manager => "manager"
    }
}

object UserLevel {
    def fromString(str: String) = str.toLowerCase match {
        case "normal" => Guest
        case "administrator" => Administrator
        case "manager" => Manager
        case _ => Guest
    }
}

object Guest extends UserLevel
object Normal extends UserLevel
object Administrator extends UserLevel
object Manager extends UserLevel

class User(ctl: Control, val username: String, val hostname: String) {
    var level: UserLevel = Guest
    var mask: String = ""

    def this(ctl: Control, prefix: Prefix) = {
        this(ctl, prefix.username, prefix.hostname)
    }

    try {
        val stmt = ctl.db.prepareStatement("SELECT mask, level FROM irc_users WHERE mask = ? OR mask = ?", username+"@"+hostname, "*@"+hostname)
        val results = stmt.executeQuery

        if (results.hasNext) {
            val row = results.firstRow
            mask  = row.getString("mask")
            level = row.getString("level") match {
                case "normal" => Normal
                case "administrator" => Administrator
                case "manager" => Manager
                case _ => Guest
            }
        }
        stmt.close
    } catch {
        case ex: Exception =>
            ctl.db.handleException(ex)
    }

    def setLevel(level: UserLevel) = {
        ctl.db.prepareStatement("UPDATE irc_users SET level = ? WHERE mask = ?", level.toString, mask).executeUpdate
    }
}


