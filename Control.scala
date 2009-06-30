package ircbot

import sql.MysqlConnection

// Main controlling class
class Control(val cfg: Config) {

    /* Wrapping around the socket to implement the IRC protocol */
    val p = new Protocol(new Connection(cfg.hostHost, cfg.hostPort))

    /* Database connection */
    val db = new MysqlConnection(cfg.dbDatabase, cfg.dbUser, cfg.dbPass)

    /* nickname of the bot */
    val nick = cfg.authNick

    /* List holding the loaded modules */
    var modulesList: List[Module] = Nil

    /* Register a specific module */
    def registerModule(module: Module) {
        modulesList = modulesList ::: module :: Nil
        module init
    }

    /* Register all default modules */
    def registerDefaultModules {
        import modules._
        registerModule(new Protocol(this))
        registerModule(new Channel(this))
        registerModule(new Manager(this))
        registerModule(new MonitorHashPHP(this))
        registerModule(new Factoids(this))

    }

    /* Display an error */
    def error(msg: String) =
        println("[!] "+msg);

    /* Dispatch any incoming messages */
    def dispatchMessage(message: Message) {
        var continue = true
        for (val module <- modulesList) if (continue) {
            continue = module handleMessage message
        }
    }

    /* Listen to incomming messages */
    def listen = {
        var continue = true
        while(continue) {
            val msg = p.readMessage
            if (msg == EOF) {
                continue = false
            }
            dispatchMessage(msg)
        }
    }

    /* call shutdown on all modules */
    def shutdownModules = {
        modulesList foreach { _ shutdown }
    }

    def start = {
        /* Process messages... */
        listen

        /* Shutdown process... */
        shutdownModules
        db.close
    }

    registerDefaultModules
}
