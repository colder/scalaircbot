package ircbot
package utils

trait HelpInfo {
  val ctl: Control

  def generalHelpInfo(access: UserLevel): Seq[(String, String)] = {
    Nil
  }

  def specificHelp: PartialFunction[(UserLevel, String), String] = Map()

}

trait SimpleHelp extends HelpInfo {
  val commandsHelp: Map[String, (Set[UserLevel], String, String)]

  override def generalHelpInfo(access: UserLevel) = {
    commandsHelp.collect{ case (cmd, (accesses, proto, desc)) if accesses contains access => (proto, desc) }.toSeq
  }

  override def specificHelp = {
    case (lvl, cmd) if commandsHelp exists { c => commandsHelp.contains(cmd) && commandsHelp(cmd)._1.contains(lvl) } =>
      val help = commandsHelp(cmd)

      help._2+": "+help._3
  }
}
