package ircbot;

case class Prefix(val nick: String, val username: String, val hostname: String) {
    val fullMask = nick+"!"+username+"@"+hostname
    val mask = "*!"+username+"@"+hostname
    val nickMask = nick+"!*@*"

    def matches(str: String) = str == fullMask && str == mask && str == nickMask;
}


object Prefix {
    def apply(str: String): Prefix = str.split("[@!]", 3).toList match {
        case nick :: user :: host :: Nil =>
            new Prefix(nick, user, host)
        case nick :: rest :: Nil =>
            if (str.indexOf("@") > 0) {
                new Prefix(nick, "", rest)
            } else {
                new Prefix(nick, rest, "")
            }
        case servername :: Nil => new Prefix("", "", servername)
        case _ => Prefix("", "", "")
    }
}



/* Different messages types */
abstract class Message;
    case class Unknown(tokens: List[String]) extends Message
    case class Error(code: Int, tokens: List[String]) extends Message
    case class Numeric(code: Int, tokens: List[String]) extends Message
    case class Msg(from: Prefix, to: String, msg: String) extends Message
    case class Mode(from: Prefix, channel: String, modes: String, user: String) extends Message
    case class Invite(from: Prefix, channel: String) extends Message
    case class Ping(msg: String) extends Message
    case object Notice extends Message
    case object EOF extends Message

class Protocol(conn: Connection) {

    /* Stores whether registration happened yet */
    var registered = false;

    def readMessage = conn.readLine match {
        case Some(l) => parseLine(l)
        case None => EOF
    }

    /*
     *  <message> ::=
     *      [':' <prefix> <SPACE> ] <command> <params> <crlf>
     *  <prefix> ::=
     *      <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
     *  <command> ::=
     *      <letter> { <letter> } | <number> <number> <number>
     *  <SPACE> ::=
     *      ' ' { ' ' }
     *  <params> ::=
     *      <SPACE> [ ':' <trailing> | <middle> <params> ]
     *  <middle> ::=
     *      <Any *non-empty* sequence of octets not including SPACE or NUL or CR or LF, the first of which may not be ':'>
     *  <trailing> ::=
     *      <Any, possibly *empty*, sequence of octets not including NUL or CR or LF>
     *  <crlf> ::=
     *      CR LF
     */
    def parseLine(line: String): Message = {

        // Stores the command part
        var command = line

        // extract the potential prefix
        val prefix = if (line startsWith ":") {
            // prefix
            val parts = line.split(" ", 2).toList
            command = parts(1)
            Some(Prefix(parts(0).substring(1)))
        } else {
            None
        }

        def args(str: String) = str.split(" +").toList

        val params = command.split(":", 2).toList match {
            case params :: lastpar :: Nil => args(params) ::: lastpar :: Nil
            case params :: Nil => args(params)
            case _ => Nil
        }

        val ret = params match {
            case "NOTICE" :: xs =>
                Notice
            case "PING" :: arg :: Nil =>
                Ping(arg)
            case "INVITE" :: nick :: to :: Nil =>
                prefix match {
                    case Some(pr) =>
                        Invite(pr, to)
                    case None => 
                        Unknown(params)
                }
            case "PRIVMSG" :: to :: msg :: Nil =>
                prefix match {
                    case Some(pr) =>
                        Msg(pr, to, msg)
                    case None =>
                        Unknown(params)
                }
            case "MODE" :: channel :: modes :: nick :: Nil =>
                prefix match {
                    case Some(pr) =>
                        Mode(pr, channel, modes, nick)
                    case None =>
                        Unknown(params)
                }

            case x :: xs =>
                try {
                    val num = Integer.parseInt(x)
                    if (num > 400) Error(num, xs) else Numeric(num, xs)
                } catch {
                    case e => Unknown(params)
                }
            case _ =>
                Unknown(params)
        }

        ret
    }

    // Standard protocol
    def quit: Unit = quit("")

    def user(username: String, hostname: String, servername: String, realname: String) =
        conn.writeLine("USER "+username+" "+hostname+" "+servername+" :"+realname)

    def pass(password: String) =
        conn.writeLine("PASS "+password)

    def nick(nickname: String) =
        conn.writeLine("NICK "+nickname)

    def quit(msg: String) =
        conn.writeLine("QUIT :"+msg)

    def join(channel: String) =
        conn.writeLine("JOIN "+channel);

    def join(channel: String, key: String) =
        conn.writeLine("JOIN "+channel+" "+key);

    def part(channel: String) =
        conn.writeLine("PART "+channel)

    def invite(nick: String, channel: String) =
        conn.writeLine("INVITE "+nick+" "+channel)

    def kick(channel: String, nick: String) =
        conn.writeLine("KICK "+channel+" "+nick)

    def mode(channel: String, mode: String, nick: String) =
        conn.writeLine("MODE "+channel+" "+mode+" "+nick)

    def op(channel: String, nick: String) =
        mode(channel, "+o", nick)

    def deop(channel: String, nick: String) =
        mode(channel, "-o", nick)

    def ban(channel: String, mask: String) =
        mode(channel, "+b", mask)

    def unban(channel: String, mask: String) =
        mode(channel, "-b", mask)

    def mute(channel: String, mask: String) =
        mode(channel, "+q", mask)

    def unmute(channel: String, mask: String) =
        mode(channel, "-q", mask)

    def kick(channel: String, nick: String, reason: String) =
        conn.writeLine("KICK "+channel+" "+nick+" :"+reason)

    def msg(to: String, msg: String) = {
        val msgl = if (msg.length > 450) msg.substring(0, 445)+"..." else msg
        conn.writeLine("PRIVMSG "+to+" :"+msgl);
    }

    def notice(to: String, msg: String) =
        conn.writeLine("NOTICE "+to+" :"+msg);

    def pong(ping: String) = 
        conn.writeLine("PONG "+ping);

}
