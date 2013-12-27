package ircbot

import utils._

/* Inner protocol between Control, Connection and the Modules */
object InnerProtocol {
  object Connected
  object Disconnected

  // Write a line to the server
  case class SendRawMessage(line: String)
  case class SendMessage(msg: Message)
  // Response from the server
  case class ReceivedMessage(msg: Message)

  case class Log(loglvl: LogLevels.LogLevel, msg: String)
}

