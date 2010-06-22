package ircbot

abstract class Duration(seconds: Int) {
    val toSeconds = seconds

    override def toString = seconds+" seconds"
}

case object Now extends Duration(0)
case class Seconds(s: Int) extends Duration(s)
case class Minutes(m: Int) extends Duration(60*m) {
    override def toString = m+" seconds"
}
case class Hours(h: Int) extends Duration(60*60*h) {
    override def toString = h+" hours"
}
case class Days(d: Int) extends Duration(24*60*60*d) {
    override def toString = d+" days"
}
