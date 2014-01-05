package ircbot
package modules

import akka.actor._
import utils._
import InnerProtocol._

class Protocol(val cfg: Config,
               val ctl: ActorRef) extends SimpleModule {

  var state = BotState(cfg.authNick)

  def newState(st: BotState) {
    state = st
  }

  override def onConnect() = {
    newState(state.copy(nick = state.origNick))
  }

  override def onDisconnect() = {
    newState(state.copy(registeredState = Unregistered))
  }

  def onMessage(msg: Message) = msg match {
    case From(_, Notice(_, _)) if state.registeredState == Unregistered =>
      // First Notice => register
      doRegister()

    case From(_, Numeric(1, _)) =>
      // Registration successful
      newState(state.copy(registeredState = Registered))

    case Numeric(`RPL_ENDOFMOTD`, _) =>
      // End of MOTD, let's join channels
      for(chan <- cfg.channels) {
        send(Join(chan))
      }

    case Error(`ERR_NOTREGISTERED`, _) => // "You have not registered"
      // Typically a reply to a ping after a reconnect
      if (state.registeredState != Unregistered) {
        // We thus re-register
        logWarning("Attempting re-registering..")
        doRegister()
      }

    case Error(`ERR_NICKNAMEINUSE`, _) => // "Nick already in use"
      logWarning("Nick is already in use!")

      newState(state.copy(nick = state.nick.nextNick))
      send(NickChange(state.nick))

    case Error(`ERR_UNAVAILRESOURCE`, _) => // "Nick is unavailable"
      logWarning("Nick is unavailable!")

      newState(state.copy(nick = state.nick.nextNick))
      send(NickChange(state.nick))

    case From(_, Ping(msg)) =>
      send(Pong(msg))

    case From(UserMask(nick, _, _), Msg(_, "test")) =>
      if (isGranted(nick, Guest)) {
        send(Msg(nick, "It works!"))
      }

    case Ping(msg) =>
      send(Pong(msg))

    case _ =>
  }


  def doRegister() {
    if (cfg.authPass != "") {
      send(Pass(cfg.authIdent.value, cfg.authPass))
    }

    send(UserC(cfg.authNick.name, cfg.authNick.name, cfg.authNick.name, cfg.authRealName))

    send(NickChange(state.nick))

    newState(state.copy(registeredState = Registering))
  }

  val RPL_ENDOFMOTD = 376
  val ERR_NOTREGISTERED = 451
  val ERR_NICKNAMEINUSE = 433
  val ERR_UNAVAILRESOURCE = 437
}
