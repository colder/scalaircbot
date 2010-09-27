package ircbot

import sql.MysqlConnection

import scala.actors.Actor
import scala.actors.Actor._

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

    /* nickname of the bot */
    var nick = cfg.authNick

    /* List holding the loaded modules */
    var modulesList: List[Module] = Nil

    /* Stores the time of the last message to detect disconnects */
    var lastMessage: Long = -1;

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
    val checker = actor {
        import InnerProtocol._

        var continue = true
        val detectionInterval = 2*60;

        while(continue) {
            // wait for kill message
            Actor.receiveWithin(detectionInterval*1000/10) {
                case StopChecker =>
                    continue = false;
            }

            if (continue) {
                val curTime = System.currentTimeMillis/1000
                if (curTime - detectionInterval > lastMessage) {
                    l.warn("Looks like we lost contact!")
                    Control.this ! ReinitConnection
                    l.warn("Reinit ordered!")
                } else if (curTime - detectionInterval/2 > lastMessage) {
                    l.warn("Trying to establish contact...")
                    c.writeLine("PING :"+curTime)
                }
            }
        }
    }

    def act() {
        import InnerProtocol._
        var continue = true
        var registering = false
        var registered  = false

        while(continue) {
            c ! ReadLine
            receive {
                case ReadLineAnswer(line) =>
                    lastMessage = System.currentTimeMillis/1000

                    val msg = p.parseLine(line)

                    // Special message handling
                    msg match {
                        case Notice if !registering && !registered =>
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

                            nick = nick+"_"

                            register(true)
                        case Error(437, _) =>
                            l.warn("Nick is unavailable!")

                            nick = nick+"_"

                            register(false)
                        case EOF =>
                            continue = false

                        case _ =>
                    }
                    dispatchMessage(msg)
                case ReinitConnection =>
                    l.warn("Reinitializing connection");
                    registered  = false;
                    registering = false;

                    try {
                        c ! ReinitConnection
                        c = new Connection(cfg.hostHost, cfg.hostPort, l)
                        c.start
                        reconnectModules
                    } catch {
                        case e: java.net.SocketTimeoutException =>
                            l.warn("Connection timeout")
                        case e: java.net.UnknownHostException =>
                            l.warn("Unknown Host: " + e.getMessage)
                        case e =>
                            l.warn("Undefined Error: " + e.getMessage)
                    }
            }
        }

        /* Shutdown process... */
        shutdown
    }

    def register(release: Boolean) {
        if (!cfg.authPass.equals("")) {
            p.pass(":"+cfg.authNick+" "+cfg.authPass)
        }
        p.user(cfg.authNick, cfg.authNick, cfg.authNick, cfg.authRealName)
        p.nick(nick)

        if (release && !cfg.authPass.equals("")) {
            nick = cfg.authNick;
            p.msg("nickserv", "release "+nick+" "+cfg.authPass)
            p.nick(nick)
        }

    }


    /* Start the connection Actor as well */
    override def start() = {
        c.start
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
