package ircbot
package modules

import akka.actor._
import utils._
import InnerProtocol._

class Protocol(val cfg: Config,
               val logger: ActorRef,
               val ctl: ActorRef) extends Module(logger, ctl) {

  abstract class RegisterState
  case object Registered extends RegisterState
  case object Registering extends RegisterState
  case object Unregistered extends RegisterState

  var registerState: RegisterState = Unregistered

  var nick: Nick = cfg.authNick

  override def onConnect() = {
    nick = cfg.authNick
  }

  override def onDisconnect() = {
    registerState = Unregistered
  }

  def onMessage(msg: Message) = msg match {
    case From(_, Notice(_, _)) if registerState == Unregistered =>
      // First Notice => register
      doRegister()

    case From(_, Numeric(1, _)) =>
      // Registration successful
      registerState = Registered

    case Numeric(`RPL_ENDOFMOTD`, _) =>
      // End of MOTD, let's join channels
      for(chan <- cfg.channels) {
        ctl ! SendMessage(Join(chan))
      }

    case Error(`ERR_NOTREGISTERED`, _) => // "You have not registered"
      // Typically a reply to a ping after a reconnect
      if (registerState != Unregistered) {
        // We thus re-register
        logWarning("Attempting re-registering..")
        doRegister()
      }

    case Error(`ERR_NICKNAMEINUSE`, _) => // "Nick already in use"
      logWarning("Nick is already in use!")

      nick = nick.nextNick
      ctl ! SendMessage(NickChange(nick))

    case Error(`ERR_UNAVAILRESOURCE`, _) => // "Nick is unavailable"
      logWarning("Nick is unavailable!")

      nick = nick.nextNick
      ctl ! SendMessage(NickChange(nick))

    case Ping(msg) =>
      ctl ! SendMessage(Pong(msg))

    case From(_, Ping(msg)) =>
      ctl ! SendMessage(Pong(msg))

    case _ =>
  }


  def doRegister() {
    if (cfg.authPass != "") {
      ctl ! SendMessage(Pass(cfg.authIdent.value, cfg.authPass))
    }

    ctl ! SendMessage(UserC(cfg.authNick.name, cfg.authNick.name, cfg.authNick.name, cfg.authRealName))

    ctl ! SendMessage(NickChange(nick))

    registerState = Registering
  }

  val RPL_ENDOFMOTD = 376
  val ERR_NOTREGISTERED = 451
  val ERR_NICKNAMEINUSE = 433
  val ERR_UNAVAILRESOURCE = 437
}
