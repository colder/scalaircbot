package ircbot

import sql.MysqlConnection

// implements the IRC protocol
class Control(host: String, port: Int, dbname: String, dbuser: String, dbpass: String) {

    /* Wrapping around the socket to implement the IRC protocol */
    val p = new Protocol(new Connection(host, port))

    /* Database connection */
    val db = new MysqlConnection(dbname, dbuser, dbpass)


    /* Main modules */
    val moduleProtocol = new modules.Protocol(this)
    val moduleChannel  = new modules.Channel(this)
    val moduleManager  = new modules.Manager(this)

    /* List holding the loaded modules */
    var modulesList: List[Module] = Nil

    def registerModule(module: Module) {
        modulesList = modulesList ::: module :: Nil
        module init
    }

    def registerDefaultModules {
        registerModule(moduleProtocol)
        registerModule(moduleChannel)
        registerModule(moduleManager)

        registerModule(new modules.MonitorHashPHP(this))
        registerModule(new modules.Factoids(this))
    }

    def error(msg: String) =
        println("[!] "+msg);

    def auth(nick: String, hostname: String, servername: String, realname: String, password: Option[String]) =
        moduleProtocol.scheduleRegistration(nick, hostname, servername, realname, password)


    def dispatchMessage(message: Message) {
        var continue = true
        for (val module <- modulesList) if (continue) {
            continue = module handleMessage message
        }
    }

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
