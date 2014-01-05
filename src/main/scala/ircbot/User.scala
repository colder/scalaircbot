package ircbot

abstract class UserLevel(val hierarchy: Int) {
}

object Guest extends UserLevel(0)
object Regular extends UserLevel(1)
object Administrator extends UserLevel(2)
object Manager extends UserLevel(3)
