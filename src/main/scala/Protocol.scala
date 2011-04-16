package ircbot;

case class Prefix(val nick: Nick, val username: String, val hostname: String) {
    val fullMask = nick.name+"!"+username+"@"+hostname
    val mask = "*!"+username+"@"+hostname
    val nickMask = nick.name+"!*@*"

    /*
    def matches(str: String) = str == fullMask && str == mask && str == nickMask;
    */
}


object Prefix {
    def apply(str: String): Prefix = str.split("[@!]", 3).toList match {
        case nick :: user :: host :: Nil =>
            new Prefix(Nick(nick), user, host)
        case nick :: rest :: Nil =>
            if (str.indexOf("@") > 0) {
                new Prefix(Nick(nick), "", rest)
            } else {
                new Prefix(Nick(nick), rest, "")
            }
        case servername :: Nil => new Prefix(Nick(""), "", servername)
        case _ => Prefix(Nick(""), "", "")
    }
}

sealed trait AbsChannel {
    val name: String
}

case class Nick(name: String) extends AbsChannel {
    def nextNick = Nick(name+"_")
}

object Nick {
    val ChanServ = Nick("ChanServ")
    val NickServ = Nick("NickServ")
}
case class Channel(name: String) extends AbsChannel


/* Different messages types */
sealed abstract class Message;
final case class Unknown(tokens: List[String]) extends Message
final case class Error(code: Int, tokens: List[String]) extends Message
final case class Numeric(code: Int, tokens: List[String]) extends Message
final case class Msg(from: Prefix, to: AbsChannel, msg: String) extends Message
final case class Mode(from: Prefix, channel: Channel, modes: String, user: String) extends Message
final case class Invite(from: Prefix, channel: Channel) extends Message
final case class Part(from: Prefix, channel: Channel) extends Message
final case class Join(from: Prefix, channel: Channel) extends Message
final case class Quit(from: Prefix) extends Message
final case class NickChange(from: Prefix, newnick: String) extends Message
final case class Ping(msg: String) extends Message
final case class Notice(from: Prefix, msg: String) extends Message

final case object EOF extends Message

class Protocol(ctl: Control) {

    def parseLine(line: Option[String]): Message = line match {
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
            case "NOTICE" :: to :: msg :: Nil =>
                prefix match {
                    case Some(pr) =>
                        Notice(pr, msg)
                    case None =>
                        Unknown(params)
                }
            case "PING" :: arg :: Nil =>
                Ping(arg)
            case "NICK" :: nick :: Nil =>
                prefix match {
                    case Some(pr) =>
                        NickChange(pr, nick)
                    case None => 
                        Unknown(params)
                }
            case "QUIT" :: _ =>
                prefix match {
                    case Some(pr) =>
                        Quit(pr)
                    case None =>
                        Unknown(params)
                }
            case "JOIN" :: channel :: _ =>
                prefix match {
                    case Some(pr) =>
                        Join(pr, Channel(channel))
                    case None =>
                        Unknown(params)
                }
            case "PART" :: channel :: _ =>
                prefix match {
                    case Some(pr) =>
                        Part(pr, Channel(channel))
                    case None =>
                        Unknown(params)
                }
            case "INVITE" :: nick :: to :: Nil =>
                prefix match {
                    case Some(pr) =>
                        Invite(pr, Channel(to))
                    case None => 
                        Unknown(params)
                }
            case "PRIVMSG" :: to :: msg :: Nil =>
                prefix match {
                    case Some(pr) =>
                        if (to startsWith "#") {
                            Msg(pr, Channel(to), msg)
                        } else {
                            Msg(pr, Nick(to), msg)
                        }
                    case None =>
                        Unknown(params)
                }
            case "MODE" :: channel :: modes :: nick :: Nil =>
                prefix match {
                    case Some(pr) =>
                        Mode(pr, Channel(channel), modes, nick)
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

    def user(user: String, hostname: String, servername: String, realname: String) =
        ctl.writeLine("USER "+user+" "+hostname+" "+servername+" :"+realname)

    def pass(password: String) =
        ctl.writeLine("PASS "+password)

    def nick(nick: Nick) =
        ctl.writeLine("NICK "+nick.name)

    def quit(msg: String) =
        ctl.writeLine("QUIT :"+msg)

    def join(channel: Channel) =
        ctl.writeLine("JOIN "+channel.name);

    def join(channel: Channel, key: String) =
        ctl.writeLine("JOIN "+channel.name+" "+key);

    def part(channel: Channel) =
        ctl.writeLine("PART "+channel.name)

    def invite(nick: String, channel: Channel) =
        ctl.writeLine("INVITE "+nick+" "+channel.name)

    def kick(channel: Channel, nick: Nick) =
        ctl.writeLine("KICK "+channel.name+" "+nick.name)

    def mode(channel: Channel, mode: String, nick: Nick) =
        ctl.writeLine("MODE "+channel.name+" "+mode+" "+nick.name)

    def mode(channel: Channel, mode: String, nick: String) =
        ctl.writeLine("MODE "+channel.name+" "+mode+" "+nick)

    def op(channel: Channel, nick: Nick) =
        mode(channel, "+o", nick.name)

    def deop(channel: Channel, nick: Nick) =
        mode(channel, "-o", nick.name)

    def ban(channel: Channel, mask: String) =
        mode(channel, "+b", mask)

    def unban(channel: Channel, mask: String) =
        mode(channel, "-b", mask)

    def mute(channel: Channel, mask: String) =
        mode(channel, "+q", mask)

    def unmute(channel: Channel, mask: String) =
        mode(channel, "-q", mask)

    def kick(channel: Channel, nick: Nick, reason: String) =
        ctl.writeLine("KICK "+channel+" "+nick.name+" :"+reason)

    def msg(to: AbsChannel, msg: String) = {
        val msgl = if (msg.length > 450) msg.substring(0, 445)+"..." else msg
        ctl.writeLine("PRIVMSG "+to.name+" :"+msgl);
    }

    def notice(to: AbsChannel, msg: String) =
        ctl.writeLine("NOTICE "+to.name+" :"+msg);

    def pong(ping: String) = 
        ctl.writeLine("PONG "+ping);

}
