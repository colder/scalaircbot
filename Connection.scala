package ircbot

import java.io._
import java.net.{InetAddress,ServerSocket,Socket,SocketException, InetSocketAddress}

import scala.actors.Actor
import scala.actors.Actor._

class Connection(host: String, port: Int, logger: Logger) extends Actor {
    val ia = InetAddress.getByName(host)
    val socket = new Socket()
    socket.connect(new InetSocketAddress(ia, port), 60*1000)
    val out = new PrintWriter(new OutputStreamWriter(socket.getOutputStream(), "UTF-8"), true)
    val in  = new BufferedReader(new InputStreamReader(socket.getInputStream()))

    var messages: List[Long] = Nil
    val timespan  = 5
    val threshold = 4

    def readLine: Option[String] = {
        val line = in.readLine
        logger.in(line)
        if (line == null) None else Some(line)
    }

    def writeLine(line: String) = {
        addMessage

        if (isFlooding) {
            logger.warn("Flood detected, delaying...");
            Thread.sleep(2000)
        }
        synchronized {
            out.println(line)
        }
        logger.out(line)

        cleanMessages
    }

    def addMessage = messages = System.currentTimeMillis :: messages
    def cleanMessages = messages = messages filter { _ > System.currentTimeMillis-timespan*2*1000 }
    def isFlooding = messages.filter{ _ > System.currentTimeMillis-timespan*1000 }.length >= threshold

    def act() {
        import InnerProtocol._

        var continue = true;

        while (continue) {
            receive {
                case ReadLine => sender ! ReadLineAnswer(readLine)
                case WriteLine(line) => writeLine(line)
                case ReinitConnection =>
                    /* Shut the connection down */
                    out.close
                    in.close
                    socket.close
                    continue = false
            }
        }
    }
}
