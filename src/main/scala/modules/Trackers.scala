package ircbot
package modules

/**
 * trait used by every module which needs to track user movements in some way
 */
trait NickTracker {
    def userJoins(p: Prefix, channel: Channel): Unit
    def userParts(p: Prefix, channel: Channel): Unit
    def userQuits(p: Prefix): Unit
    def userRenames(p: Prefix, newnick: Nick): Unit
}

class Trackers(ctl: Control) extends Module(ctl) {
    var nickTrackers = Set[NickTracker]()

    def registerNickTracker(nt: NickTracker) = nickTrackers += nt

    def unregisterNickTracker(nt: NickTracker) = nickTrackers -= nt

    def handleMessage(msg: Message) = {
        msg match {
            case Part(pr, channel) =>
                nickTrackers.foreach(_.userParts(pr, channel))

            case Join(pr, channel) =>
                nickTrackers.foreach(_.userJoins(pr, channel))

            case Quit(pr) =>
                nickTrackers.foreach(_.userQuits(pr))

            case NickChange(pr, newnick) =>
                nickTrackers.foreach(_.userRenames(pr, Nick(newnick)))
        }
        true
    }
}
