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
        case "normal" => Normal
        case "administrator" => Administrator
        case "manager" => Manager
        case _ => Guest
    }
}

object Guest extends UserLevel
object Normal extends UserLevel
object Administrator extends UserLevel
object Manager extends UserLevel

class User(ctl: Control, val nick: Nick) {
    def this(ctl: Control, prefix: Prefix) = {
        this(ctl, prefix.nick)
    }

    val ident = ctl.idents.getIdent(nick)
    val level = ctl.idents.getAuth(nick)
}


