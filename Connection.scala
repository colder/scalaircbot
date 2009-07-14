package ircbot

import java.io._
import java.net.{InetAddress,ServerSocket,Socket,SocketException}

class Connection(host: String, port: Int, logger: Logger) {
    val ia = InetAddress.getByName(host)
    val socket = new Socket(ia, port)
    var out = new PrintWriter(new OutputStreamWriter(socket.getOutputStream(), "UTF-8"), true)
    var in = new BufferedReader(new InputStreamReader(socket.getInputStream()))

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

        out.println(line)
        logger.out(line)

        cleanMessages
    }

    def addMessage = messages = System.currentTimeMillis :: messages
    def cleanMessages = messages = messages filter { _ > System.currentTimeMillis-timespan*2*1000 }
    def isFlooding = messages.filter{ _ > System.currentTimeMillis-timespan*1000 }.length >= threshold
}
