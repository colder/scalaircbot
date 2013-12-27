package ircbot;

import scala.xml._
import akka.actor._

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
        usage
    } else {
        val system  = ActorSystem("ircbot")
        val control = system.actorOf(Props(new Control(new Config(args(0)))), name = "control")
    }
  }

  def usage = {
    println("Usage: ircbot <config.xml>");
  }
}
