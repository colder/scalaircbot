package ircbot.modules.helpers

import ircbot._

trait Commands {

    def words(str: String): List[String] = words(str, 0)

    def words(str: String, limit: Int): List[String] =
        str.split("[:,. ]", limit).toList


}
