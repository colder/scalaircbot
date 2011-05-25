package ircbot
package modules

import utils.Commands
import utils.HelpInfo

class HelpProvider(val ctl: Control) extends Module(ctl) with Commands {
  def handleMessage(msg: Message) = {
    msg match {
      case Msg(prefix, to: Nick, msg) =>
        words(msg, 2) match {
          case "!help" :: Nil | "help" :: Nil =>
            provideGeneralHelp(prefix)
            false
          case "!help" :: cmd :: Nil =>
            provideHelp(prefix, cmd)
            false
          case "help"  :: cmd :: Nil =>
            provideHelp(prefix, cmd)
            false
          case _ =>
            true
        }
      case _ =>
        true
    }
  }

  def provideGeneralHelp(prefix: Prefix) = {
    val access = ctl.idents.getAuth(prefix.nick)

    val infos = ctl.modulesList.collect{ case m: HelpInfo => m.generalHelpInfo(access) }.flatten.sortBy(_._1)

    val maxSize = infos.map(_._1.length).max

    for ((k, v) <- infos) {
      ctl.p.notice(prefix.nick, String.format("%-"+maxSize+"s : %s", k, v))
    }
  }

  def provideHelp(prefix: Prefix, cmd: String) = {
    val access = ctl.idents.getAuth(prefix.nick)

    val query = if (cmd.startsWith("!")) {
      cmd.substring(1)
    } else {
      cmd
    }

    val helpFound = ctl.modulesList.collect { case m: HelpInfo if m.specificHelp.isDefinedAt((access, query)) => m.specificHelp((access, query)) }

    if (helpFound.isEmpty) {
      ctl.p.notice(prefix.nick, "No specific help found for '"+query+"'")
    } else {
      for (line <- helpFound) {
        ctl.p.notice(prefix.nick, line )
      }
    }
  }
}
