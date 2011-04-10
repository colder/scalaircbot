package ircbot
/**
 * trait used by every module which needs to track user movements in some way
 */
trait NickTracker {
    def userJoins(p: Prefix, channel: Channel): Unit
    def userParts(p: Prefix, channel: Channel): Unit
    def userQuits(p: Prefix): Unit
    def userRenames(p: Prefix, newnick: Nick): Unit
}
