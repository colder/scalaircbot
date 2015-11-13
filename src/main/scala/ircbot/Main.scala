package ircbot;

import akka.actor._

import com.typesafe.akka.extension.quartz.QuartzSchedulerExtension
import InnerProtocol._

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
        usage
    } else {
        Config.forConfig(args(0)) match {
          case Some(cfg) =>
            val system  = ActorSystem("ircbot")
            val control = system.actorOf(Props(new Control(cfg)), name = "control")

            val scheduler = QuartzSchedulerExtension(system)

            scheduler.schedule("Ticker", control, Tick)
            scheduler.schedule("GC", control, GC)

            control ! Init
          case None =>
            println("Configuration not found for run-mode: "+args(0));
        }
    }
  }

  def usage = {
    println("Usage: ircbot <config.xml>");
  }
}
