package ircbot

abstract class Module(ctl: Control) {
    def init = {};
    def handleMessage(message: Message): Boolean;
    def shutdown = {};
    def reconnect = {};

    implicit var onReplies = collection.mutable.Map[Long, PartialFunction[Message, Unit]]()
}
