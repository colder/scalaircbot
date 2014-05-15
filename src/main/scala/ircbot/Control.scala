package ircbot

import utils._
import akka.actor._

import scala.slick.driver.MySQLDriver.simple._
import org.apache.commons.dbcp2.BasicDataSource
import modules._

import InnerProtocol._

// Main controlling class
class Control(val cfg: Config) extends Actor with RemoteLogger {
  /* Connection actor used to send/receive messages */
  var c: ActorRef  = null // Connection

  val ctl = self

  val logger = context.actorOf(Props(new Logger(cfg)))

  /* Database connection */
  val dataSource = {
    val ds = new BasicDataSource
    ds.setUsername(cfg.dbUser)
    ds.setPassword(cfg.dbPass)
    ds.setMaxIdle(5);
    ds.setInitialSize(1);
    ds.setValidationQuery("SELECT 1 FROM irc_factoids")
    ds.setUrl("jdbc:mysql://" + cfg.dbHost+ ":" + cfg.dbPort + "/"+cfg.dbDatabase)
    ds
  }

  val db = Database.forDataSource(dataSource);

  var modules: Map[String, ActorRef] = Map(
    "protocol" -> context.actorOf(Props(new Protocol(cfg, self))),
    "auth"     -> context.actorOf(Props(new Auth(db, self))),
    "factoids" -> context.actorOf(Props(new Factoids(db, self)))
  )

  def dispatch(f: ActorRef => Unit) {
    modules.foreach{ case (name, mod) => f(mod) }
  }

  def receive = {
    case Connected =>
      dispatch(_ ! Connected)

    case Disconnected =>
      dispatch(_ ! Disconnected)

    case GC =>
      logInfo("Performing GC")
      dispatch(_ ! GC)

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

    case ReceivedMessage(msg) =>
      dispatch(_ ! ReceivedMessage(msg))

    case SendMessage(msg) =>
      c ! SendMessage(msg)

    case SendRawMessage(data) =>
      c ! SendRawMessage(data)
  }

  override def preStart = {
    initializeConnection()
  }

  def initializeConnection() {
    val cName = "connection"+ConnectionCounter.getNext
    c = context.actorOf(Props(new Connection(cfg.hostHost, cfg.hostPort, logger, self, cName)), name = cName)
  }

//  /* Special chanserv module used to perform delayed OP Actions */
//  var chanserv: modules.Chanserv = null
//  var trackers: modules.Trackers = null
//  var banlog:   modules.BanLog   = null
//  var factoids: modules.Factoids = null
//
//  /* Store for Nick->Idents relationships */
//  var idents: modules.Idents = null
//
//  /* nickname of the bot */
//  var nick = cfg.authNick
//
//  /* List holding the loaded modules */
//  var modulesList: List[Module] = Nil
//
//  import InnerProtocol._
//
//  abstract class RegisterState
//  case object Registered extends RegisterState
//  case object Registering extends RegisterState
//  case object Unregistered extends RegisterState
//
//  var registerState: RegisterState = Unregistered
//
//  override def preStart = {
//    init
//  }
//
//  def init {
//    try {
//      initializeConnection()
//
//      l.info("Connecting to Database...")
//      db       = new MysqlConnection(cfg.dbHost, cfg.dbPort, cfg.dbDatabase, cfg.dbUser, cfg.dbPass)
//
//      l.info("Loading ChanServ Module...")
//      chanserv = new modules.Chanserv(this)
//
//      l.info("Loading Trackers Module...")
//      trackers = new modules.Trackers(this)
//
//      l.info("Loading Ident Module...")
//      /* Freenode Idents attached to Nicks */
//      idents   = new modules.Idents(this)
//      trackers.registerNickTracker(idents)
//
//      l.info("Loading BanLog Module...")
//      banlog   = new modules.BanLog(this)
//
//      l.info("Loading Factoid Module...")
//      factoids = new modules.Factoids(this)
//
//      registerDefaultModules()
//
//      l.info("Connecting to IRC server...")
//      c ! Start
//
//    } catch {
//      case e: Throwable =>
//        l.err("Unnexpected error: "+e);
//        shutdown
//        throw e
//    }
//  }
//
//  def initializeConnection() {
//      val cName = "connection"+ConnectionCounter.getNext
//      c = context.actorOf(Props(new Connection(cfg.hostHost, cfg.hostPort, l, cName)), name = cName)
//  }
//
//  def getContext = context
//
//  /* Register a specific module */
//  def registerModule(module: Module) {
//    modulesList = modulesList ::: module :: Nil
//    module.init
//  }
//
//  /* Register all default modules */
//  def registerDefaultModules() {
//    import modules._
//    registerModule(new Protocol(this))
//    registerModule(new HelpProvider(this))
//    registerModule(chanserv)
//    registerModule(trackers)
//    registerModule(idents)
//    registerModule(banlog)
//    registerModule(new Manager(this))
//    registerModule(new MonitorHashPHP(this))
//    registerModule(new RussianRoulette(this))
//    //registerModule(new Yahoo(this))
//    registerModule(factoids)
//  }
//
//  /* Display an error */
//  def error(msg: String) = l.err(msg)
//
//  /* Dispatch any incoming messages */
//  def dispatchMessage(message: Message) {
//    var continue = true
//    for (module <- modulesList) if (continue) {
//      continue = module handleMessage message
//    }
//  }
//
//  def receive = {
//    case Connected =>
//
//    case Disconnected =>
//
//    case ReadLine(line) =>
//      val msg = p.parseLine(line)
//
//      // Special message handling
//      msg match {
//        case _: Notice if registerState == Unregistered =>
//          // First Notice => register
//          doRegister()
//
//        case Numeric(1, _) =>
//          // Registration successful
//          registerState = Registered
//
//        case Numeric(376, _) =>
//          // End of MOTD, let's join channels
//          for(chan <- cfg.channels) {
//            p.join(chan)
//          }
//
//          if (nick != cfg.authNick) {
//            // if we had to change nick, let's ask for a release now
//            doRelease()
//          }
//
//        case Error(451, _) => // "You have not registered"
//          // Typically a reply to a ping after a reconnect
//          if (registerState != Unregistered) {
//            // We thus re-register
//            l.warn("Attempting re-registering..")
//            doRegister()
//
//            modulesList.foreach(_.reconnect)
//          }
//
//        case Error(433, _) => // "Nick already in use"
//          l.warn("Nick is already in use!")
//
//          nick = nick.nextNick
//          p.nick(nick)
//
//        case Error(437, _) => // "Nick is unavailable"
//          l.warn("Nick is unavailable!")
//
//          nick = nick.nextNick
//          p.nick(nick)
//
//        case _ =>
//      }
//
//      dispatchMessage(msg)
//
//    case ReinitConnection =>
//      l.err("Reinitializing connection...")
//      c ! akka.actor.PoisonPill
//      initializeConnection()
//
//  }
//
//  override def postStop() {
//    super.postStop()
//
//    /* Shutdown process... */
//    shutdown()
//  }
//
//  def shutdown() {
//    l.info("Shutting down...");
//
//    modulesList.foreach(_.shutdown)
//
//    if (db != null) {
//      db.close
//    }
//
//    l.info("Bye");
//  }
//
//
//  def doRegister() {
//    if (cfg.authPass != "") {
//      p.pass(":"+cfg.authIdent.value+" "+cfg.authPass)
//    }
//
//    p.user(cfg.authNick.name, cfg.authNick.name, cfg.authNick.name, cfg.authRealName)
//
//    p.nick(nick)
//
//    registerState = Registering
//  }
//
//  def doRelease() {
//    assert(cfg.authPass != "")
//
//    p.msg(Nick.NickServ, "ghost "+cfg.authNick.name+" "+cfg.authPass)
//  }
}
