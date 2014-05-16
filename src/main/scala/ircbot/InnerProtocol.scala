package ircbot

import utils._
import org.joda.time.Duration
import db.BanTypes.BanType

/* Inner protocol between Control, Connection and the Modules */
object InnerProtocol {
  // Initialization
  case object Init

  case object Connected
  case object Disconnected

  // Communication between modules
  case class SendTo(module: String, msg: Any)
  case class Dispatch(msg: AnyRef, sendToSender: Boolean = false)

  // Garbage collection to allow modules to perform GC tasks
  case object GC
  case object Tick

  // Write a line to the server
  case class SendRawMessage(line: String)
  case class SendMessage(msg: Message)
  // Response from the server
  case class ReceivedMessage(msg: Message)

  case class Log(loglvl: LogLevels.LogLevel, msg: String)

  // Listen/reply
  case class ListenUntil(onMessage: PartialFunction[Any, Any])

  // Auth/Idents
  case class AuthGetUser(n: Nick)

  // Help
  case class HelpEntries(entries: Seq[HelpEntry])

  // OP Request
  case class RequestOp(chan: Channel)

  case class RequestBan(tpe: BanType, user: Nick, duration: Duration, reason: String)

  case object RequestBotState
}

