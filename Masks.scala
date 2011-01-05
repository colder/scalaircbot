package ircbot

import scala.collection.immutable.Map
import scala.util.matching.Regex

case class Mask(pattern: String) {
    val regex = "^"+pattern.replace(".", "\\.").replace("*", ".*")+"$";
    def matchAgainst(username: String, hostname: String): Boolean = {
        val target = username+"@"+hostname

        new Regex(regex).findFirstIn(target) != None
    }
}


class MaskStore(ctl: Control) {
    private var loaded = false;

    var masks = Map[Mask, UserLevel]()


    def load(implicit force: Boolean = false) = if (!loaded || force) {
        loaded = true;

        try {
            val stmt = ctl.db.prepareStatement("SELECT mask, level FROM irc_users")
            val results = stmt.executeQuery

            results.foreach { row =>
                val mask = Mask(row.getString("mask"));
                val level = row.getString("level") match {
                    case "normal" => Normal
                    case "administrator" => Administrator
                    case "manager" => Manager
                    case _ => Guest
                }

                masks = masks + (mask -> level)
            }

            stmt.close
        } catch {
            case ex: Exception =>
                ctl.db.handleException(ex)
        }
    }

    def reload = load(true)

    def get(username: String, hostname: String): (Mask, UserLevel) = {
        load

        masks.find { case (mask, ul) => mask.matchAgainst(username, hostname)}.getOrElse((Mask(""), Guest))
    }
}

