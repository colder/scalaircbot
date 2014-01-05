package ircbot

abstract class RegisterState
case object Registered extends RegisterState
case object Registering extends RegisterState
case object Unregistered extends RegisterState

case class BotState(
  origNick: Nick,
  nick: Nick,
  modes: Map[Channel, Set[Modes.ChannelUserMode]] = Map(),
  registeredState: RegisterState = Unregistered
)

object BotState {
  def apply(origNick: Nick): BotState = BotState(origNick, origNick)
}
