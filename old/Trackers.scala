package ircbot
package modules

/**
 * trait used by every module which needs to track user movements in some way
 */
trait NickTracker {
  def onUserJoin(p: Prefix, channel: Channel): Unit
  def onUserPart(p: Prefix, channel: Channel): Unit
  def onUserQuit(p: Prefix): Unit
  def onUserRename(p: Prefix, newnick: Nick): Unit
}

class Trackers(ctl: Control) extends Module(ctl) {
  var nickTrackers = Set[NickTracker]()

  def registerNickTracker(nt: NickTracker) = nickTrackers += nt

  def unregisterNickTracker(nt: NickTracker) = nickTrackers -= nt

  def handleMessage(msg: Message) = {
    msg match {
      case Part(pr, channel) =>
        nickTrackers.foreach(_.onUserPart(pr, channel))

      case Join(pr, channel) =>
        nickTrackers.foreach(_.onUserJoin(pr, channel))

      case Quit(pr) =>
        nickTrackers.foreach(_.onUserQuit(pr))

      case NickChange(pr, newnick) =>
        nickTrackers.foreach(_.onUserRename(pr, Nick(newnick)))

      case _ =>
    }
    true
  }
}
