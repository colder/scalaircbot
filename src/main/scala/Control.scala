package ircbot

import sql.MysqlConnection

import utils._
import akka.actor._

// Main controlling class
class Control(val cfg: Config) extends Actor {
  val l = new TerminalColorLogger();

  /* Connection actor used to send/receive messages */
  var c: ActorRef = null // Connection

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

  /* Stores the time of the last message to detect disconnects */
  var lastMessage: Long = -1;

  /* Stores the time of the last message to detect disconnects */
  var connected: Boolean = false;

  import InnerProtocol._

  var registering = false
  var registered  = false

  override def preStart = {
    init

    c ! StartListening
  }

  def init {
    try {
      c        = context.actorOf(Props(new Connection(cfg.hostHost, cfg.hostPort, l)), name = "c")

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
      idents = new modules.Idents(this)
      trackers.registerNickTracker(idents)

      l.info("Loading BanLog Module...")
      banlog = new modules.BanLog(this)

      l.info("Loading Factoid Module...")
      factoids = new modules.Factoids(this)

      registerDefaultModules()

      l.info("Connecting to IRC server...")
      c ! Start

    } catch {
      case e =>
        l.err("Unnexpected error: "+e);
        shutdown
        throw e
    }
  }

  /* Register a specific module */
  def registerModule(module: Module) {
    modulesList = modulesList ::: module :: Nil
    module init
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
      lastMessage = System.currentTimeMillis/1000

      val msg = p.parseLine(line)

      // Special message handling
      msg match {
        case _: Notice if !registering && !registered =>
          // First Notice => register
          register(false)
          registering = true
        case Numeric(1, _) =>
          // Registration successful
          registered = true
          registering = false
        case Numeric(376, _) =>
          // End of MOTD, let's join channels
          for(chan <- cfg.channels)
            p.join(chan)
        case Error(433, _) =>
          l.warn("Nick is already in use!")

          nick = nick.nextNick

          register(true)
        case Error(437, _) =>
          l.warn("Nick is unavailable!")

          nick = nick.nextNick

          register(false)
        case EOF =>
          l.err("Connection EOF, should reconnect soon...")
        case _ =>
      }

      dispatchMessage(msg)
    case ReinitConnection =>
      l.warn("Blup connection");
  }

  override def postStop() {
    super.postStop()

    c ! StopListening

    /* Shutdown process... */
    shutdown()
  }

  def shutdown() {
    l.info("Shutting down...");

    shutdownModules

    if (db != null) {
      db.close
    }

    l.info("Bye");
  }


  def register(release: Boolean) {
    if (release && !cfg.authPass.equals("")) {
      nick = cfg.authNick;
      p.msg(Nick.NickServ, "release "+nick.name+" "+cfg.authPass)
      p.nick(nick)
    } else {
      if (!cfg.authPass.equals("")) {
        p.pass(":"+cfg.authIdent.value+" "+cfg.authPass)
      }
      p.user(cfg.authNick.name, cfg.authNick.name, cfg.authNick.name, cfg.authRealName)
      p.nick(nick)
    }
  }

  /* Send a message */
  def writeLine(line: String) {
    c ! InnerProtocol.WriteLine(line)
  }

  /* call shutdown on all modules */
  def shutdownModules = {
    modulesList foreach { _ shutdown }
  }

  /* Inform modules that we got reconnected */
  def reconnectModules = {
    modulesList foreach { _ reconnect }
  }
}
