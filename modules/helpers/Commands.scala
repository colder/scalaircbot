package ircbot.modules.helpers

trait Commands {

    def words(str: String): List[String] = words(str, 0)

    def words(str: String, limit: Int): List[String] =
        str.split("[:,. ]", limit).toList


}
