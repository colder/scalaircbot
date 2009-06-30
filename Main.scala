package ircbot;

import scala.xml._

object Main {
    def main(args: Array[String]): Unit = {
        if (args.length < 1) {
            usage
        } else {
            val bot = new Control(new Config(args(0)))
            bot start
        }
    }

    def usage = {
        println("Usage: ircbot <config.xml>");
    }
}
