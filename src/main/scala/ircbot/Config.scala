package ircbot;

import com.typesafe.config._
import scala.collection.JavaConversions._

case class Config(
  serverHost: String,
  serverPort: Int,

  authNick: Nick,
  authIdent: Ident,
  authPass: String,
  authRealName: String,

  channels: List[Channel]
)

object Config {
  def forConfig(key: String): Option[Config] = {
    try {
      val cfg = ConfigFactory.load().getConfig(key)

      Some(Config(
        serverHost   = cfg.getString("irc.server.host"),
        serverPort   = cfg.getInt("irc.server.port"),

        authNick     = Nick(cfg.getString("irc.auth.nick")),
        authIdent    = Ident(cfg.getString("irc.auth.ident")),
        authPass     = cfg.getString("irc.auth.pass"),
        authRealName = cfg.getString("irc.auth.realname"),

        channels     = cfg.getStringList("irc.perform").toList.map(Channel(_))
      ))
    } catch {
      case _: RuntimeException =>
        None
    }
  }
}
