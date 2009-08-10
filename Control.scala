package ircbot

import sql.MysqlConnection

import scala.actors.Actor
import scala.actors.Actor._

// Main controlling class
class Control(val cfg: Config) extends Actor {
    val l = new TerminalColorLogger();

    /* Connection actor used to send/receive messages */
    var c = new Connection(cfg.hostHost, cfg.hostPort, l)

    /* Wrapping around the socket to implement the IRC protocol */
    val p = new Protocol(this)

    /* Database connection */
    val db = new MysqlConnection(cfg.dbDatabase, cfg.dbUser, cfg.dbPass)

    /* Special chanserv module used to perform delayed OP Actions */
    val chanserv = new modules.Chanserv(this)

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
        for (val module <- modulesList) if (continue) {
            continue = module handleMessage message
        }
    }

    /* Asynchronous checks of the connection */
    val checker = actor {
        import InnerProtocol._

        val checkInterval = 10*60;
        val detectionInterval = 20*60;

        while(true) {
            Thread.sleep(checkInterval*1000);

            val curTime = System.currentTimeMillis/1000
            if (curTime - detectionInterval/2 > lastMessage) {
                l.warn("Trying to establish contact...");
                c.writeLine("PING :"+curTime)
            } else if (curTime - detectionInterval > lastMessage) {
                l.warn("Looks like we lost contact!");
                this ! ReinitConnection
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
                            register
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
                            if (nick endsWith "_") {
                                nick = nick.substring(0, nick.length-1)
                            } else {
                                nick = nick+"_"
                            }
                            register
                        case EOF =>
                            continue = false

                        case _ =>
                    }
                    dispatchMessage(msg)
                case ReinitConnection => 
                    l.warn("Reinitializing connection");
                    registered  = false;
                    registering = false;

                    c ! ReinitConnection
                    c = new Connection(cfg.hostHost, cfg.hostPort, l)
                    c.start
                    reconnectModules
            }
        }

        /* Shutdown process... */
        shutdownModules
        db.close
    }

    def register() {
        if (!cfg.authPass.equals("")) {
            p.pass(cfg.authPass)
        }
        p.user(nick, nick, nick, cfg.authRealName)
        p.nick(nick)

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

    registerDefaultModules
}
