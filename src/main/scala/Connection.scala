package ircbot

import java.io._
import java.net.{InetAddress,ServerSocket,Socket,SocketException, InetSocketAddress}

import scala.actors.Actor
import scala.actors.OutputChannel
import scala.actors.Actor._

import scala.collection.mutable.Queue

import utils.Logger

import InnerProtocol._

class Connection(host: String, port: Int, logger: Logger) extends Actor {
    val socket = new Socket()
    var out: PrintWriter   = null
    var in: BufferedReader = null

    var messages: List[Long] = Nil
    val timespan  = 5
    val threshold = 4

    type Listener = OutputChannel[Any]
    var listeners = Set[Listener]()
    var buffers   = Map[Listener, Queue[Option[String]]]().withDefaultValue(Queue())
    var reading   = Set[Listener]()

    def init {
        val ia = InetAddress.getByName(host);

        socket.connect(new InetSocketAddress(ia, port), 15*1000)
        out = new PrintWriter(new OutputStreamWriter(socket.getOutputStream(), "UTF-8"), true)
        in  = new BufferedReader(new InputStreamReader(socket.getInputStream()))
    }

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

        var continue = true;
        var opened   = false;

        while (continue) {
            if (opened) {
              // Read lines from the connection, if any
              while (in.ready()) {
                  val newline = readLine
                  for (l <- listeners) {
                      buffers(l).enqueue(newline)
                  }
              }


              val readingQueue = reading.toSeq
              for (r <- readingQueue) {
                if (!buffers(r).isEmpty) {
                  r ! ReadLineAnswer(buffers(r).dequeue)
                  reading -= r
                }
              }
            }


            receiveWithin(1000) {
                case InitConnection =>
                    init
                    opened = true
                case StartListening =>
                    listeners += sender
                case StopListening =>
                    listeners -= sender
                    buffers   -= sender

                case ReadLine =>
                    if (reading(sender)) {
                      logger.err("Received ReadLine from an actor already reading: "+sender)
                    } else {
                      reading += sender;
                    }

                case WriteLine(line) =>
                    writeLine(line)

                case CloseConnection =>
                    /* Shut the connection down */
                    out.close
                    in.close
                    socket.close
                    continue = false
                case _ =>
            }
        }
    }
}
