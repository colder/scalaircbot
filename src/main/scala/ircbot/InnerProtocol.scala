package ircbot

import utils._
import org.joda.time.Duration
import db.BanTypes.BanType

/* Inner protocol between Control, Connection and the Modules */
object InnerProtocol {
  // Initialization
  case object Init

  abstract class Event
  case object Connected extends Event
  case object Disconnected extends Event
  case object GC extends Event
  case object Tick extends Event

  // Actions sent to control
  case object Reconnect
  case class SendTo(module: String, msg: Any)
  case class Dispatch(msg: AnyRef, sendToSender: Boolean = false)
  case class SendRawMessage(line: String)
  case class SendMessage(msg: Message)
  case class Log(loglvl: LogLevels.LogLevel, msg: String)

  // Help
  case class AuthGetUser(n: Nick)
  case class HelpEntries(entries: Seq[HelpEntry])
  case class RequestOp(chan: Channel)
  case class RequestBan(tpe: BanType, user: Nick, duration: Duration, reason: String)
  case object RequestBotState
}

