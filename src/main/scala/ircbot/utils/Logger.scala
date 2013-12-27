package ircbot
package utils

import InnerProtocol.Log

import akka.actor._
import java.text.SimpleDateFormat
import java.util.Date

class Logger(cfg: Config) extends Actor {
  import LogLevels._

  def receive = {
    case Log(lvl, msg) =>
      val format = lvl match {
        case OUT  => "%s \033[34m[>]\033[0m %s"
        case IN   => "%s \033[32m[<]\033[0m %s"
        case ERR  => "%s \033[31m[!]\033[0m %s"
        case WARN => "%s \033[33m[w]\033[0m %s"
        case INFO => "%s \033[32m[i]\033[0m %s"
      }

      val df = new SimpleDateFormat("dd-MMM-yy HH:mm:ss")
      def date = df.format(new Date())

      println(format.format(date, msg))
  }
}

object LogLevels {
  sealed abstract class LogLevel

  case object OUT  extends LogLevel
  case object IN   extends LogLevel
  case object ERR  extends LogLevel
  case object WARN extends LogLevel
  case object INFO extends LogLevel

}

trait RemoteLogger {
  import LogLevels._

  val logger: ActorRef

  def logOut(msg: String)     = log(OUT,  msg)
  def logIn(msg: String)      = log(IN,   msg)
  def logError(msg: String)   = log(ERR,  msg)
  def logWarning(msg: String) = log(WARN, msg)
  def logInfo(msg: String)    = log(INFO, msg)

  def log(level: LogLevel, msg: String) = {
    logger ! Log(level, msg)
  }
}
