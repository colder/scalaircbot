package ircbot;

import scala.xml._

object Main {
    def main(args: Array[String]): Unit = {
        if (args.length < 1) {
            usage
        } else {
            var bot: Control = null
            try {
                bot = new Control(new Config(args(0)))
                bot start
            } catch {
                case _ =>
            }
        }
    }

    def usage = {
        println("Usage: ircbot <config.xml>");
    }
}
