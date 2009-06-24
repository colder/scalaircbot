package ircbot.modules.helpers

trait Auth {

    def isGranted(ctl: Control, prefix: Prefix, levels: UserLevel*): Boolean = {
        val user = new User(ctl, prefix)
        levels.toList.exists(_ == user.level)
    }

    def userLevel(ctl: Control, prefix: Prefix) = new User(ctl, prefix).level
}
