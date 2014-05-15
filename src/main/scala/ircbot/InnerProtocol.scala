package ircbot

import utils._

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
  case object HelpCollect
  case class HelpEntries(entries: Seq[HelpEntry])
}

