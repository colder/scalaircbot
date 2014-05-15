package ircbot;

import scala.xml._
import akka.actor._

import com.typesafe.akka.extension.quartz.QuartzSchedulerExtension
import InnerProtocol._

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
        usage
    } else {
        val system  = ActorSystem("ircbot")
        val control = system.actorOf(Props(new Control(new Config(args(0)))), name = "control")

        val scheduler = QuartzSchedulerExtension(system)

        scheduler.schedule("Ticker", control, Tick)
        scheduler.schedule("GC", control, GC)

        control ! Init
    }
  }

  def usage = {
    println("Usage: ircbot <config.xml>");
  }
}
