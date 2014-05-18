package ircbot
package modules

import akka.actor._
import utils._
import InnerProtocol._

class Protocol(val cfg: Config,
               val ctl: ActorRef) extends SimpleModule {

  var state = BotState(cfg.authNick)

  override def onConnect() = {
    state = state.copy(nick = cfg.authNick)
  }

  override def onDisconnect() = {
    state = state.copy(registeredState = Unregistered)
  }

  def onMessage(msg: Message) = msg match {
    case From(_, Notice(_, _)) if state.registeredState == Unregistered =>
      // First Notice => register
      doRegister()

    case From(_, Numeric(1, _)) =>
      // Registration successful
      state = state.copy(registeredState = Registered)

    case From(NickMask(nick), Invite(_, chan)) =>
      requireGranted(nick, Administrator) {
        send(Join(chan))
      }

    case From(_, Numeric(`RPL_ENDOFMOTD`, _)) =>
      // End of MOTD, let's join channels
      for(chan <- cfg.channels) {
        send(Join(chan))
      }

    case From(_, Error(`ERR_NOTREGISTERED`, _)) => // "You have not registered"
      // Typically a reply to a ping after a reconnect
      if (state.registeredState != Unregistered) {
        // We thus re-register
        logWarning("Attempting re-registering..")
        doRegister()
      }

    case From(_, Error(`ERR_NICKNAMEINUSE`, _)) => // "Nick already in use"
      logWarning("Nick is already in use!")

      state = state.copy(lastTried = state.lastTried.nextNick)

      if (state.lastTried != state.nick) {
        send(NickChange(state.lastTried))
      }

      if (state.registeredState != Registered) {
        state = state.copy(nick = state.lastTried)
        logInfo("Current bot nick set to "+state.lastTried.name)
      }

    case From(NickMask(nick), NickChange(newnick)) =>
      if (nick == state.nick) {
        state = state.copy(nick = newnick)
        logInfo("Current bot nick set to "+newnick.name)
      }


    case From(_, Error(`ERR_UNAVAILRESOURCE`, _)) => // "Nick is unavailable"
      logWarning("Nick is unavailable!")

      state = state.copy(lastTried = state.lastTried.nextNick)

      if (state.lastTried != state.nick) {
        send(NickChange(state.lastTried))
      }

      if (state.registeredState != Registered) {
        state = state.copy(nick = state.lastTried)
        logInfo("Current bot nick set to "+state.lastTried.name)
      }

    case From(_, Ping(msg)) =>
      send(Pong(msg))

    case Ping(msg) =>
      send(Pong(msg))


    case _ =>
  }

  override def receive = {
    case RequestBotState =>
      sender ! state

    case Tick =>
      if (state.nick != cfg.authNick) {
        // Try to go back to original nick
        state = state.copy(lastTried = cfg.authNick)
        send(NickChange(cfg.authNick))
      }

    case m =>
      super.receive(m)
  }


  def doRegister() {
    if (cfg.authPass != "") {
      send(Pass(cfg.authIdent.value, cfg.authPass))
    }

    send(UserC(cfg.authNick.name, cfg.authNick.name, cfg.authNick.name, cfg.authRealName))

    send(NickChange(state.nick))

    state = state.copy(registeredState = Registering)
  }

  val RPL_ENDOFMOTD = 376
  val ERR_NOTREGISTERED = 451
  val ERR_NICKNAMEINUSE = 433
  val ERR_UNAVAILRESOURCE = 437
}
