package ircbot
package utils

import InnerProtocol.Log

import akka.actor._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

class Logger(cfg: Config) extends Actor {
  import LogLevels._

  val format = DateTimeFormat.forPattern("dd-MMM-yy HH:mm:ss")

  def receive = {
    case Log(lvl, msg) =>
      val icon = lvl match {
        case OUT  => Console.CYAN    + "[>]" + Console.RESET
        case IN   => Console.GREEN   + "[<]" + Console.RESET
        case ERR  => Console.RED     + "[!]" + Console.RESET
        case WARN => Console.YELLOW  + "[w]" + Console.RESET
        case INFO => Console.MAGENTA + "[i]" + Console.RESET
      }

      val date = format.print(new DateTime())

      println(f"$date $icon $msg")
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

  val ctl: ActorRef

  def logOut(msg: String)     = log(OUT,  msg)
  def logIn(msg: String)      = log(IN,   msg)
  def logError(msg: String)   = log(ERR,  msg)
  def logWarning(msg: String) = log(WARN, msg)
  def logInfo(msg: String)    = log(INFO, msg)

  def log(level: LogLevel, msg: String) = {
    ctl ! Log(level, msg)
  }
}
