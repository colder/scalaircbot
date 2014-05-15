package ircbot
package utils

import akka.actor._

trait HelpInfo {
  def helpEntries: Seq[HelpEntry] = Nil
}

case class HelpEntry(minLevel: UserLevel, name: String, command: String, description: String)
