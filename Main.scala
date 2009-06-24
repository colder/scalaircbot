package ircbot;

import scala.xml._

object Main {
    def main(args: Array[String]): Unit = {
        if (args.length < 1) {
            usage
        } else {
            val data = XML.loadFile(args(0))

            val db = data \ "db" first
            val auth = data \ "auth" first
            val host = data \ "host" first

            val dbUser = (db \ "@user").text
            val dbPass = (db \ "@pass").text
            val dbDatabase = (db \ "@database").text

            val hostHost = (host \ "@host").text
            val hostPort = Integer.parseInt((host \ "@port").text)

            val authNick = (auth \ "@nick").text
            val authPass = (auth \ "@pass").text

            val bot = new Control(hostHost, hostPort, dbDatabase, dbUser, dbPass)
            bot auth(authNick, authNick, authNick, "Mr Bot", Some(authPass))
            bot start
        }
    }

    def usage = {
        println("Usage: ircbot <config.xml>");
    }
}
