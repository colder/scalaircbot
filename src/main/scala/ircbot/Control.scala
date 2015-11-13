package ircbot

import utils._
import akka.actor._

import slick.driver.MySQLDriver.api._
import modules._

import InnerProtocol._

// Main controlling class
class Control(val cfg: Config) extends Actor with RemoteLogger {
  /* Connection actor used to send/receive messages */
  var c: ActorRef  = null // Connection

  val ctl = self

  val logger = context.actorOf(Props(new Logger(cfg)))

  /* Database connection */
  val db = Database.forConfig("db");

  var modules: Map[String, ActorRef] = Map(
    "protocol" -> context.actorOf(Props(new Protocol(cfg, self))),
    "auth"     -> context.actorOf(Props(new Auth(db, self))),
    "factoids" -> context.actorOf(Props(new Factoids(db, self))),
    "acl"      -> context.actorOf(Props(new ACL(db, self))),
    "op"       -> context.actorOf(Props(new OpControl(self))),
    "banlog"   -> context.actorOf(Props(new BanLog(db, self, cfg.channels.head))),
    "flood"    -> context.actorOf(Props(new FloodProtect(self, cfg.channels.head))),
    "help"     -> context.actorOf(Props(new Help(self))),
    "concheck" -> context.actorOf(Props(new ConnectionChecker(self)))
  )

  def dispatch(f: ActorRef => Unit) {
    modules.foreach{ case (name, mod) => f(mod) }
  }

  var firstTick = true

  def receive = {
    case Init =>
      initializeConnection()
      dispatch(_ ! Init)

    case Connected =>
      dispatch(_ ! Connected)

    case Disconnected =>
      dispatch(_ ! Disconnected)
      logInfo("Reconnecting in 5 seconds...")
      Thread.sleep(5000)
      initializeConnection()

    case Reconnect =>
      dispatch(_ ! Disconnected)
      context.stop(c)
      initializeConnection()


    case GC =>
      dispatch(_ ! GC)

    case Tick =>
      // First tick do not propagate to avoid tick-before-connect
      if (firstTick) {
        firstTick = false
      } else {
        dispatch(_ ! Tick)
      }

    case Dispatch(msg, toSender) =>
      modules.collect {
        case (name, mod) if toSender || (mod != sender) =>
          mod forward msg
      }

    case SendTo(module, msg) =>
      if (modules contains module) {
        modules(module) forward msg
      }

    case l : Log =>
      logger forward l

    case msg: Message =>
      dispatch(_ ! msg)

    case SendMessage(msg) =>
      c ! SendMessage(msg)

    case SendRawMessage(data) =>
      c ! SendRawMessage(data)
  }

  def initializeConnection() {
    val cName = "connection"+ConnectionCounter.getNext
    c = context.actorOf(Props(new Connection(cfg.serverHost, cfg.serverPort, logger, self, cName)), name = cName)
  }
}
