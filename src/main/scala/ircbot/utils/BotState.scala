package ircbot

abstract class RegisterState
case object Registered extends RegisterState
case object Registering extends RegisterState
case object Unregistered extends RegisterState

case class BotState(
  nick: Nick,
  registeredState: RegisterState = Unregistered
)

object BotState {
  def apply(origNick: Nick): BotState = BotState(origNick, Unregistered)
}
