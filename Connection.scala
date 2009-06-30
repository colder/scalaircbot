package ircbot

import java.io._
import java.net.{InetAddress,ServerSocket,Socket,SocketException}

class Connection(host: String, port: Int, logger: Logger) {
    val ia = InetAddress.getByName(host)
    val socket = new Socket(ia, port)
    var out = new PrintWriter(new OutputStreamWriter(socket.getOutputStream(), "UTF-8"), true)
    var in = new BufferedReader(new InputStreamReader(socket.getInputStream()))

    def readLine: Option[String] = {
        val line = in.readLine
        logger.in(line)
        if (line == null) None else Some(line)
    }

    def writeLine(line: String) = {
        out.println(line)
        logger.out(line)
    }
}
