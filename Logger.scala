package ircbot

import java.text.SimpleDateFormat
import java.util.Date

abstract class Logger {
    def out(msg: String);
    def in(msg: String);
    def err(msg: String);
}

class TerminalLogger extends Logger {
    val df = new SimpleDateFormat("dd-MMM-yy HH:mm:ss")

    def out(msg: String) = println(df.format(new Date())+" [>] "+msg)
    def in(msg: String)  = println(df.format(new Date())+" [<] "+msg)
    def err(msg: String) = println(df.format(new Date())+" [!] "+msg)
}
