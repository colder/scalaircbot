package ircbot

import akka.actor._

abstract class Mask {
  val onick: Option[Nick]
  val username: Option[String]
  val hostname: Option[String]

  override def toString = { 
    onick.map(_.name).getOrElse("*")+"!"+username.getOrElse("*")+"@"+hostname.getOrElse("*")
  }
}

case class UserMask(nick: Nick, username: Option[String], hostname: Option[String]) extends Mask {
  val onick = Some(nick)

  def onlyNickMask = UserMask(nick, None, None)
}

object NickMask {
  def unapply(m: Mask): Option[(Nick)] = m match {
    case UserMask(nick, _, _) => Some((nick))
    case _ => None
  }
}

case class ServerMask(_hostname: String) extends Mask {
  val onick = None
  val username = None
  val hostname = Some(_hostname)
}

case object WildcardMask extends Mask {
  val onick = None
  val username = None
  val hostname = None
}

case class AccountMask(account: String) extends Mask {
  val onick = None
  val username = None
  val hostname = None

  override def toString = "$a:"+account
}


object Mask {
  def apply(str: String): Mask = {
    if (str.startsWith("$a:")) {
      AccountMask(str.substring(3, str.length))
    } else {
      str.split("[@!]", 3).toList match {
        case nick :: user :: host :: Nil =>
          UserMask(Nick(nick), Some(user), Some(host))

        case nick :: rest :: Nil =>
          if (str.indexOf("@") > 0) {
            UserMask(Nick(nick), None, Some(rest))
          } else {
            UserMask(Nick(nick), Some(rest), None)
          }

        case servername :: Nil =>
          ServerMask(servername)

        case _ =>
          WildcardMask
      }
    }
  }
}

sealed trait AbsChannel {
  val name: String
}

object AbsChannel {
  def apply(txt: String): AbsChannel = {
    if (txt.startsWith("#")) {
      Channel(txt)
    } else {
      Nick(txt)
    }
  }
}

object Modes {
  sealed abstract class ChannelUserMode(val v: String) {
    override def toString = v
  }

  case class O(nick: Nick) extends ChannelUserMode("o "+nick.name)
  case class V(nick: Nick) extends ChannelUserMode("v "+nick.name)
  case class B(mask: Mask) extends ChannelUserMode("b "+mask)
  case class Q(mask: Mask) extends ChannelUserMode("q "+mask)

  sealed abstract class Mode(v: String) {
    override def toString = v
  }
  case class Plus(m: ChannelUserMode)  extends Mode("+"+m)
  case class Minus(m: ChannelUserMode) extends Mode("-"+m)
}


sealed trait UserID

case class Nick(name: String) extends AbsChannel with UserID {
  def nextNick = Nick(name+"_")
}

case class Ident(val value: String) extends UserID {
  def toMask: String = "$a:"+value
}

object Nick {
  val ChanServ = Nick("ChanServ")
  val NickServ = Nick("NickServ")
}
case class Channel(name: String) extends AbsChannel


/* Different messages types */
sealed abstract class Message

case class From(mask: Mask, msg: Message) extends Message
case class Unknown(tokens: List[String]) extends Message
case class Error(code: Int, tokens: List[String]) extends Message
case class Numeric(code: Int, tokens: List[String]) extends Message
case class Msg(to: AbsChannel, msg: String) extends Message
case class Mode(channel: Channel, modes: Seq[Modes.Mode]) extends Message
case class Invite(nick: Nick, channel: Channel) extends Message
case class Part(channel: Channel) extends Message
case class Join(channel: Channel) extends Message
case class Quit(optMessage: Option[String]) extends Message
case class NickChange(nick: Nick) extends Message
case class Ping(msg: String) extends Message
case class Pong(msg: String) extends Message
case class Notice(to: AbsChannel, msg: String) extends Message
case class UserC(user: String, hostname: String, servername: String, realname: String) extends Message
case class Pass(ident: String, pass: String) extends Message


object IrcHelpers {
  def messageToRawCommand(msg: Message): Option[String] = msg match {
    case Msg(to, msg) =>
      val msgl = if (msg.length > 450) msg.substring(0, 445)+"..." else msg
      Some(s"PRIVMSG ${to.name} :$msgl");

    case Mode(channel, modes) =>
      Some(s"MODE ${channel.name} ${modes.mkString(" ")}");

    case Invite(user, channel) =>
      Some(s"INVITE ${user.name} ${channel.name}");

    case Part(channel) =>
      Some(s"PART ${channel.name}");

    case Join(channel) =>
      Some(s"JOIN ${channel.name}");

    case Quit(optMessage) =>
      Some(s"QUIT ${optMessage.getOrElse("")}");

    case NickChange(newnick) =>
      Some(s"NICK ${newnick.name}");

    case Ping(msg) =>
      Some(s"PING ${msg}");

    case Pong(msg) =>
      Some(s"PONG ${msg}");

    case Notice(to, msg) =>
      Some(s"NOTICE ${to.name} ${msg}");

    case Pass(ident, password) =>
      Some(s"PASS :$ident $password")

    case UserC(user, hostname, servername, realname) =>
      Some(s"USER $user $hostname $servername :$realname")

    case _ =>
      None
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
  def rawCommandToMessage(line: String): Message = {
    // Stores the command part
    var command = line

    // extract the potential prefix
    val prefix = if (line startsWith ":") {
        // prefix
        val parts = line.split(" ", 2).toList
        command = parts(1)
        Some(Mask(parts(0).substring(1)))
    } else {
        None
    }

    def args(str: String) = str.split(" +").toList

    val params = command.split(":", 2).toList match {
        case params :: lastpar :: Nil => args(params) ::: lastpar :: Nil
        case params :: Nil => args(params)
        case _ => Nil
    }

    def optFrom(msg: Message): Message = prefix match {
      case Some(pr) => From(pr, msg)
      case None => msg
    }

    val ret = params match {
      case "NOTICE" :: to :: msg :: Nil =>
        optFrom(Notice(AbsChannel(to), msg))

      case "PING" :: arg :: Nil =>
        optFrom(Ping(arg))

      case "NICK" :: nick :: Nil =>
        optFrom(NickChange(Nick(nick)))

      case "QUIT" :: optMessage =>
        optFrom(Quit(optMessage.headOption))

      case "JOIN" :: channel :: _ =>
        optFrom(Join(Channel(channel)))

      case "PART" :: channel :: _ =>
        optFrom(Part(Channel(channel)))

      case "INVITE" :: nick :: to :: Nil =>
        optFrom(Invite(Nick(nick), Channel(to)))

      case "PRIVMSG" :: to :: msg :: Nil =>
        optFrom(Msg(AbsChannel(to), msg))

      case "MODE" :: channel :: modesString :: nickMask :: Nil =>
        import Modes._

        // TODO generalize
        val m = modesString match {
          case "+o" => Some(Plus( O(Nick(nickMask))))
          case "-o" => Some(Minus(O(Nick(nickMask))))
          case "+v" => Some(Plus( V(Nick(nickMask))))
          case "-v" => Some(Minus(V(Nick(nickMask))))
          case "+b" => Some(Plus( B(Mask(nickMask))))
          case "-b" => Some(Minus(B(Mask(nickMask))))
          case "+q" => Some(Plus( Q(Mask(nickMask))))
          case "-q" => Some(Minus(Q(Mask(nickMask))))
          case _ => None
        }

        m match {
          case Some(m) =>
            optFrom(Mode(Channel(channel), List(m)))
          case _ =>
            optFrom(Unknown(params))
        }

      case x :: xs =>
        optFrom(try {
            val num = Integer.parseInt(x)
            if (num > 400) {
              Error(num, xs)
            } else {
              Numeric(num, xs)
            }
          } catch {
            case _: NumberFormatException =>
              Unknown(params)
          })

      case _ =>
        optFrom(Unknown(params))
    }

    ret
  }
}
