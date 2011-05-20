package ircbot
package utils

import java.util.regex.Pattern

abstract class Duration(seconds: Int) {
    val toSeconds = seconds

    override def toString = seconds+" seconds"
}

case object Now extends Duration(0)
case class Seconds(s: Int) extends Duration(s)
case class Minutes(m: Int) extends Duration(60*m) {
    override def toString = m+" minutes"
}
case class Hours(h: Int) extends Duration(60*60*h) {
    override def toString = h+" hours"
}
case class Days(d: Int) extends Duration(24*60*60*d) {
    override def toString = d+" days"
}

object ExDuration {
  def unapply(str: String): Option[Duration] = {
    val p = Pattern.compile("(\\d+)([dhms])")

    val matcher = p.matcher(str)
    if (matcher.find()) {
      val amount = matcher.group(1).toInt
      matcher.group(2) match {
        case "d" =>
          Some(Days(amount))
        case "h" =>
          Some(Hours(amount))
        case "m" =>
          Some(Minutes(amount))
        case "s" =>
          Some(Seconds(amount))
        case _ =>
          None
      }
    } else {
      None
    }
  }
}
