package ircbot

import sql.MysqlConnection

import utils._
import akka.actor._
import akka.actor.SupervisorStrategy._
import scala.concurrent.duration._
import language.postfixOps

// Main controlling class
class Control(val cfg: Config) extends Actor {
  val l = new TerminalColorLogger();

  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 5, withinTimeRange = 10 minutes) {
    case _: ConnectionClosedException => Restart
    case _: ActorKilledException => Restart
  }

  /* Connection actor used to send/receive messages */
  var c: ActorRef  = null // Connection
  var cc: ActorRef = null // ConnectionChecker

  /* Wrapping around the socket to implement the IRC protocol */
  var p: Protocol = null

  /* Database connection */
  var db: MysqlConnection = null

  /* Special chanserv module used to perform delayed OP Actions */
  var chanserv: modules.Chanserv = null
  var trackers: modules.Trackers = null
  var banlog: modules.BanLog     = null
  var factoids: modules.Factoids = null

  /* Store for Nick->Idents relationships */
  var idents: modules.Idents = null

  /* nickname of the bot */
  var nick = cfg.authNick

  /* List holding the loaded modules */
  var modulesList: List[Module] = Nil

  import InnerProtocol._

  abstract class RegisterState
  case object Registered extends RegisterState
  case object Registering extends RegisterState
  case object Unregistered extends RegisterState

  var registerState: RegisterState = Unregistered

  override def preStart = {
    init
  }

  def init {
    try {
      cc = context.actorOf(Props(new ConnectionChecker(10.seconds, l)), name = "conchecker")

      initializeConnection()

      l.info("Loading Protocol...")
      p        = new Protocol(this)

      l.info("Connecting to Database...")
      db       = new MysqlConnection(cfg.dbHost, cfg.dbPort, cfg.dbDatabase, cfg.dbUser, cfg.dbPass)

      l.info("Loading ChanServ Module...")
      chanserv = new modules.Chanserv(this)

      l.info("Loading Trackers Module...")
      trackers = new modules.Trackers(this)

      l.info("Loading Ident Module...")
      /* Freenode Idents attached to Nicks */
      idents   = new modules.Idents(this)
      trackers.registerNickTracker(idents)

      l.info("Loading BanLog Module...")
      banlog   = new modules.BanLog(this)

      l.info("Loading Factoid Module...")
      factoids = new modules.Factoids(this)

      registerDefaultModules()

      l.info("Connecting to IRC server...")
      c ! Start

    } catch {
      case e: Throwable =>
        l.err("Unnexpected error: "+e);
        shutdown
        throw e
    }
  }

  def initializeConnection() {
      if (c ne null) {
        cc ! UntrackConnection(c)
      }

      val cName = "connection"+ConnectionCounter.getNext
      c = context.actorOf(Props(new Connection(cfg.hostHost, cfg.hostPort, l, cName)), name = cName)

      cc ! TrackConnection(c)
  }

  def getContext = context

  /* Register a specific module */
  def registerModule(module: Module) {
    modulesList = modulesList ::: module :: Nil
    module.init
  }

  /* Register all default modules */
  def registerDefaultModules() {
    import modules._
    registerModule(new Protocol(this))
    registerModule(new HelpProvider(this))
    registerModule(chanserv)
    registerModule(trackers)
    registerModule(idents)
    registerModule(banlog)
    registerModule(new Manager(this))
    registerModule(new MonitorHashPHP(this))
    registerModule(new RussianRoulette(this))
    //registerModule(new Yahoo(this))
    registerModule(factoids)
  }

  /* Display an error */
  def error(msg: String) = l.err(msg)

  /* Dispatch any incoming messages */
  def dispatchMessage(message: Message) {
    var continue = true
    for (module <- modulesList) if (continue) {
      continue = module handleMessage message
    }
  }

  def receive = {
    case ReadLine(line) =>
      val msg = p.parseLine(line)

      // Special message handling
      msg match {
        case _: Notice if registerState == Unregistered =>
          // First Notice => register
          doRegister()

        case Numeric(1, _) =>
          // Registration successful
          registerState = Registered

        case Numeric(376, _) =>
          // End of MOTD, let's join channels
          for(chan <- cfg.channels) {
            p.join(chan)
          }

          if (nick != cfg.authNick) {
            // if we had to change nick, let's ask for a release now
            doRelease()
          }

        case Error(451, _) => // "You have not registered"
          // Typically a reply to a ping after a reconnect
          if (registerState == Registered) {
            // We thus re-register
            doRegister()

            modulesList.foreach(_.reconnect)
          }

        case Error(433, _) => // "Nick already in use"
          l.warn("Nick is already in use!")

          nick = nick.nextNick
          p.nick(nick)

        case Error(437, _) => // "Nick is unavailable"
          l.warn("Nick is unavailable!")

          nick = nick.nextNick
          p.nick(nick)

        case EOF =>
          l.err("Connection EOF, should reconnect soon...")
        case _ =>
      }

      dispatchMessage(msg)
    case ReinitConnection =>
      c ! akka.actor.PoisonPill

      initializeConnection()

  }

  override def postStop() {
    super.postStop()

    c ! StopListening

    /* Shutdown process... */
    shutdown()
  }

  def shutdown() {
    l.info("Shutting down...");

    modulesList.foreach(_.shutdown)

    if (db != null) {
      db.close
    }

    l.info("Bye");
  }


  def doRegister() {
    if (cfg.authPass != "") {
      p.pass(":"+cfg.authIdent.value+" "+cfg.authPass)
    }

    p.user(cfg.authNick.name, cfg.authNick.name, cfg.authNick.name, cfg.authRealName)

    p.nick(nick)

    registerState = Registering
  }

  def doRelease() {
    assert(cfg.authPass != "")

    p.msg(Nick.NickServ, "ghost "+cfg.authNick.name+" "+cfg.authPass)
  }

  /* Send a message */
  def writeLine(line: String) {
    c ! InnerProtocol.WriteLine(line)
  }
}
