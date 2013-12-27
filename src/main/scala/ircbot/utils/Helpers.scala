package ircbot
package utils

import java.util.{Date,Calendar}
import java.text.SimpleDateFormat

object Helpers {
  def dateAsString(date: Date) = {
    val df = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss")
    df.format(date)
  }
}
