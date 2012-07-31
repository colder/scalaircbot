package ircbot

abstract class Module(ctl: Control) {
  def init = {}
  def handleMessage(message: Message): Boolean;
  def shutdown = {}
  def reconnect = {}
}
