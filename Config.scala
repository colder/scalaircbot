package ircbot;

import scala.xml._


class Config(path: String) {
    private val data = XML.loadFile(path)
    private val db = data \ "db" first
    val dbUser = (db \ "@user").text
    val dbPass = (db \ "@pass").text
    val dbDatabase = (db \ "@database").text

    private val host = data \ "host" first
    val hostHost = (host \ "@host").text
    val hostPort = Integer.parseInt((host \ "@port").text)

    private val auth = data \ "auth" first
    val authNick = (auth \ "@nick").text
    val authPass = (auth \ "@pass").text
    val authRealName = (auth \ "@realname").text

    val channels = { (data \ "perform" \ "channel") map { _.text} }
}
