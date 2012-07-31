package ircbot
package utils

import java.text.SimpleDateFormat
import java.util.Date

abstract class Logger {
  def out(msg: String);
  def in(msg: String);
  def err(msg: String);
  def warn(msg: String);
  def info(msg: String);
}

class TerminalLogger extends Logger {
  val df = new SimpleDateFormat("dd-MMM-yy HH:mm:ss")
  def date = df.format(new Date())

  def out(msg: String)  = println(date+" [>] "+msg)
  def in(msg: String)   = println(date+" [<] "+msg)
  def err(msg: String)  = println(date+" [!] "+msg)
  def warn(msg: String) = println(date+" [w] "+msg)
  def info(msg: String) = println(date+" [i] "+msg)
}

class TerminalColorLogger extends TerminalLogger {
  override def out(msg: String) = println(date+" \033[34m[>]\033[0m "+msg)
  override def in(msg: String)  = println(date+" \033[32m[<]\033[0m "+msg)
  override def err(msg: String) = println(date+" \033[31m[!]\033[0m "+msg)
  override def warn(msg: String) = println(date+" \033[33m[w]\033[0m "+msg)
  override def info(msg: String) = println(date+" \033[32m[i]\033[0m "+msg)
}
