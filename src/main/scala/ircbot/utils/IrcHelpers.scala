package ircbot;

import akka.actor._

trait IrcHelpersOld {
  val ctl: ActorRef

  // Standard protocol
  def quit: Unit = quit("")

  def user(user: String, hostname: String, servername: String, realname: String) =
    writeLine("USER "+user+" "+hostname+" "+servername+" :"+realname)

  def pass(password: String) =
    writeLine("PASS "+password)

  def nick(nick: Nick) =
    writeLine("NICK "+nick.name)

  def quit(msg: String) =
    writeLine("QUIT :"+msg)

  def join(channel: Channel) =
    writeLine("JOIN "+channel.name);

  def join(channel: Channel, key: String) =
    writeLine("JOIN "+channel.name+" "+key);

  def part(channel: Channel) =
    writeLine("PART "+channel.name)

  def invite(nick: String, channel: Channel) =
    writeLine("INVITE "+nick+" "+channel.name)

  def kick(channel: Channel, nick: Nick) =
    writeLine("KICK "+channel.name+" "+nick.name)

  def mode(channel: Channel, mode: String, nick: Nick) =
    writeLine("MODE "+channel.name+" "+mode+" "+nick.name)

  def mode(channel: Channel, mode: String, nick: String) =
    writeLine("MODE "+channel.name+" "+mode+" "+nick)

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
    writeLine("KICK "+channel.name+" "+nick.name+" :"+reason)

  def msg(to: AbsChannel, msg: String) = {
    val msgl = if (msg.length > 450) msg.substring(0, 445)+"..." else msg
    writeLine("PRIVMSG "+to.name+" :"+msgl);
  }

  def notice(to: AbsChannel, msg: String) =
    writeLine("NOTICE "+to.name+" :"+msg);

  def pong(ping: String) = 
    writeLine("PONG "+ping);

  def writeLine(msg: String) {
    ctl ! InnerProtocol.SendRawMessage(msg)
  }

}
