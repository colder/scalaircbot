package ircbot

import sql.MysqlConnection

import scala.actors.Actor
import scala.actors.Actor._
import utils._

// Main controlling class
class Control(val cfg: Config) extends Actor {
    val l = new TerminalColorLogger();

    /* Connection actor used to send/receive messages */
    var c: Connection = null

    /* Wrapping around the socket to implement the IRC protocol */
    var p: Protocol = null

    /* Database connection */
    var db: MysqlConnection = null

    /* Special chanserv module used to perform delayed OP Actions */
    var chanserv: modules.Chanserv = null
    var trackers: modules.Trackers = null

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

    /* Register a specific module */
    def registerModule(module: Module) {
        modulesList = modulesList ::: module :: Nil
        module init
    }

    /* Register all default modules */
    def registerDefaultModules {
        import modules._
        registerModule(new Protocol(this))
        registerModule(chanserv)
        registerModule(trackers)
        registerModule(idents)
        registerModule(new Manager(this))
        registerModule(new MonitorHashPHP(this))
        registerModule(new RussianRoulette(this))
        registerModule(new Yahoo(this))
        registerModule(new Factoids(this))

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

    /* Asynchronous checks of the connection */
    var checker: ConnectionChecker = null

    def act() {
        import InnerProtocol._
        var continue = true
        var registering = false
        var registered  = false

        c ! StartListening
        while(continue) {
            c ! ReadLine
            receive {
                case ReadLineAnswer(line) =>
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
                            continue = false

                        case _ =>
                    }
                    dispatchMessage(msg)
                case ReinitConnection =>
                    l.warn("Reinitializing connection");
                    lastMessage = System.currentTimeMillis/1000
                    registered  = false;
                    registering = false;

                    try {
                        connected = false;
                        c ! ReinitConnection
                        l.info("Connecting to IRC server...")
                        c = new Connection(cfg.hostHost, cfg.hostPort, l)
                        c.start
                        reconnectModules
                        connected = true
                    } catch {
                        case e: java.net.SocketTimeoutException =>
                            l.warn("Connection timeout")
                            connected = false
                        case e: java.net.UnknownHostException =>
                            l.warn("Unknown Host: " + e.getMessage)
                        case e =>
                            l.warn("Undefined Error: " + e.getMessage)
                    }
            }
        }
        c ! StopListening

        /* Shutdown process... */
        shutdown
    }

    def register(release: Boolean) {

        if (release && !cfg.authPass.equals("")) {
            nick = cfg.authNick;
            p.msg(Nick.NickServ, "release "+nick.name+" "+cfg.authPass)
            p.nick(nick)
        } else {
            if (!cfg.authPass.equals("")) {
                p.pass(":"+cfg.authNick.name+" "+cfg.authPass)
            }
            p.user(cfg.authNick.name, cfg.authNick.name, cfg.authNick.name, cfg.authRealName)
            p.nick(nick)
        }

    }

    /* Start the connection Actor as well */
    override def start() = {
        c.start
        connected = true;
        super.start
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

    def init {
        try {
            l.info("Connecting to IRC server...")
            c        = new Connection(cfg.hostHost, cfg.hostPort, l)

            l.info("Loading Protocol...")
            p        = new Protocol(this)

            l.info("Connecting to Database...")
            db       = new MysqlConnection(cfg.dbHost, cfg.dbPort, cfg.dbDatabase, cfg.dbUser, cfg.dbPass)

            l.info("Loading ChanServ Module...")
            chanserv = new modules.Chanserv(this)

            l.info("Loading Trackers Module...")
            trackers = new modules.Trackers(this)

            /* Freenode Idents attached to Nicks */
            idents = new modules.Idents(this)
            trackers.registerNickTracker(idents)

            checker = new ConnectionChecker(this)
            checker start

            registerDefaultModules
        } catch {
            case e =>
                l.err("Unnexpected error: "+e);
                shutdown
                throw e
        }
    }

    def shutdown {
        l.info("Shutting down...");
        if (checker != null) {
            checker ! InnerProtocol.StopChecker
        }

        shutdownModules

        if (db != null) {
            db.close
        }
        l.info("Bye");
    }

    init
}
