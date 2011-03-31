package ircbot
import InnerProtocol._
import scala.actors.Actor
import scala.actors.Actor._


class ConnectionChecker(ctl: Control) extends Actor {
    def act() = {
        ctl.l.info("Starting connection monitoring...")
        var continue = true
        val detectionInterval = 10*60;

        while(continue) {
            // wait for kill message
            receiveWithin(detectionInterval*1000/10) {
                case StopChecker =>
                    ctl.l.info("Stopping connection monitoring...")
                    continue = false;
                case _ =>
            }

            if (continue) {
                val curTime = System.currentTimeMillis/1000
                if (curTime - detectionInterval > ctl.lastMessage) {
                    ctl.l.warn("Looks like we lost contact!")
                    ctl ! ReinitConnection
                    ctl.l.warn("Reinit ordered!")
                } else if (curTime - detectionInterval/2 > ctl.lastMessage && ctl.connected) {
                    ctl.l.warn("Trying to establish contact...")
                    ctl.c.writeLine("PING :"+curTime)
                }
            }

        }

        ctl.l.info("Connection monitoring shutdown")

    }
}
