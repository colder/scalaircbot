package ircbot
package modules

import utils._

class Chanserv(val ctl: Control) extends Module(ctl) with Commands {
  var isOP       = Set[Channel]()
  var isRequestingOP = Set[Channel]()
  var opActions    = Map[Channel, List[() => Unit]]().withDefaultValue(List())

  def handleMessage(msg: Message) = {
    msg match {
      case Mode(prefix, channel, modes, user) if user == ctl.cfg.authNick.name =>
        if (modes contains "+o") {
          isOP       += channel
          isRequestingOP -= channel
        } else if (modes contains "-o") {
          isOP       -= channel
        }
      case _ =>
    }

    checkActionsList()

    true
  }

  private def checkActionsList() = {
    for((channel, acts) <- opActions if isOP(channel)) {
    for (a <- acts) {
      a()
    }

    opActions += channel -> Nil

    isOP -= channel
    ctl.p.deop(channel, ctl.nick);
    }
  }

  def afterOP(channel: Channel)(action: () => Unit) =
    if (isOP(channel)) {
    action()
    } else {
    opActions = opActions + (channel -> (opActions(channel) ::: action :: Nil))
    }

  def op(channel: Channel) = {
    if (!isOP(channel) && !isRequestingOP(channel)) {
    isRequestingOP += channel
    ctl.p.msg(Nick.ChanServ, "OP "+channel.name)
    }
  }

  def doAsOP(channel: Channel)(action: => Unit) = {
    afterOP(channel)(() =>action)
    op(channel)
  }

  override def reconnect = {
    isOP       = Set[Channel]()
    isRequestingOP = Set[Channel]()
  }
}
