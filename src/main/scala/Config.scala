package ircbot;

import scala.xml._


class Config(path: String) {
    private val data = XML.loadFile(path)
    private val db = data \ "db" head
    val dbUser = (db \ "@user").text
    val dbPass = (db \ "@pass").text
    val dbDatabase = (db \ "@database").text
    val dbHost = (db \ "@host").text
    val dbPort = (db \ "@port").text.toInt

    private val host = data \ "host" head
    val hostHost = (host \ "@host").text
    val hostPort = Integer.parseInt((host \ "@port").text)

    private val auth = data \ "auth" head
    val authNick = Nick((auth \ "@nick").text)
    val authPass = (auth \ "@pass").text
    val authRealName = (auth \ "@realname").text

    val channels = { (data \ "perform" \ "channel") map { c => Channel(c.text)} }
}
