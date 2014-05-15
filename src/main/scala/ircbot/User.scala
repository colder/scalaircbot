package ircbot

case class User(n: Nick, level: UserLevel) {
  
}

object User {
  def default(nick: Nick) = User(nick, Guest)
}

abstract class UserLevel(val hierarchy: Int) extends Ordered[UserLevel] {
  def compare(that: UserLevel) = this.hierarchy - that.hierarchy
}

case object Guest extends UserLevel(0) {
  override def toString = "guest"
}
case object Regular extends UserLevel(1) {
  override def toString = "regular"
}
case object Administrator extends UserLevel(2) {
  override def toString = "admin"
}
case object Manager extends UserLevel(3) {
  override def toString = "big boss"
}
