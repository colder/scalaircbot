package ircbot
package modules

import utils._

class MonitorHashPHP(val ctl: Control) extends Module(ctl) with Commands with SimpleHelp {
    val channel = Channel("##php")
    val floodTimespan = 4
    val floodThreshold = 5
    val profanityTimespan = 2880
    val profanityThreshold = 3

    val shortMessagesThresold   = 3
    val shortMessagesBufferSize = 5
    val shortMessagesTimespan   = 30

    var messages      = Map[Nick, List[Long]]().withDefaultValue(Nil)
    var profanity     = Map[Nick, List[(String, Long)]]().withDefaultValue(Nil)
    var shortMessages = Map[Nick, List[(Int, Long)]]().withDefaultValue(Nil)

    var lastCleanup = 0

    def handleMessage(msg: Message) = msg match {
        case Msg(from, to, msg) =>
            if (to equals channel) {
                // Check for profanity
                if (isProfanity(msg)) {
                    addProfanity(from.nick, msg)
                    if (!isGranted(from, Manager, Administrator)) {
                        if (isAbusingProfanity(from.nick)) {
                            mute(from, Minutes(30), "profanity")
                        } else {
                            ctl.p.msg(from.nick, "Please keep the profanity out of "+channel+", thanks.")
                        }
                    }
                }


                // checks that no nick sends more than <floodThreshold> msg per <floodTimespan> sec
                addMessage(from.nick)

                if (isFlooding(from.nick)) {
                    if (!isGranted(from, Regular, Manager, Administrator)) {
                        mute(from, Minutes(1), "mute")
                    }
                    messages -= from.nick
                } else {
                    // check that messages are of decent length
                    addShortMessage(from.nick, msg)

                    if (isAbusingEnter(from.nick)) {
                        if (!isGranted(from, Regular, Manager, Administrator)) {
                            ctl.p.msg(from.nick, "Please stop using your enter key as punctuation, thanks.")
                        }
                        shortMessages -= from.nick
                    }
                }

                cleanup

                true
            } else {
                words(msg, 3) match {
                    case "!profanity" :: "add" :: word :: Nil =>
                        requireAuth(from, Manager, Administrator) {
                            val updated = ctl.db.prepareStatement("INSERT INTO irc_profanity SET word = ?", word).executeUpdate
                            ctl.p.msg(from.nick, "Word '"+word+"' registered as profanity.")
                        }
                        false

                    case "!profanity" :: "remove" :: word :: Nil =>
                        requireAuth(from, Manager, Administrator) {
                            val upadted = ctl.db.prepareStatement("DELETE FROM irc_profanity WHERE word = ?", word).executeUpdate
                            if (upadted > 0) {
                                ctl.p.msg(from.nick, "Word '"+word+"' removed as profanity.")
                            } else {
                                ctl.p.msg(from.nick, "Word '"+word+"' not found.")
                            }
                        }
                        false

                    case "!profanity" :: "list" :: Nil =>
                        requireAuth(from, Manager, Administrator) {
                            val results = ctl.db.prepareStatement("SELECT DISTINCT word FROM irc_profanity").executeQuery
                            var l: List[String] = Nil;
                            for (r <- results) l = l ::: r.getString("word") :: Nil
                            ctl.p.msg(from.nick, "List: "+l.mkString(", "))
                        }
                        false
                    case _ => true
                }
            }
        case _ => true
    }

    def now = System.currentTimeMillis

    def addMessage(nick: Nick) =
        messages += nick -> (now :: messages(nick))

    def countRealWords(msg: String) = {
        words(msg.replaceAll("[^a-zA-Z0-9 ]+", "")).size
    }

    def addShortMessage(nick: Nick, msg: String) =
        shortMessages += nick -> ((countRealWords(msg), now) :: shortMessages(nick).take(shortMessagesBufferSize-1))

    def isAbusingEnter(nick: Nick) = {
        val msgs = shortMessages(nick).filter{ _._2 > now-shortMessagesTimespan*1000 }.map(_._1)

        if (msgs.size == shortMessagesBufferSize) {
            msgs.foldRight(0)(_ + _) < shortMessagesThresold * msgs.size
        } else {
            false
        }
    }

    def addProfanity(nick: Nick, msg: String) =
        profanity += nick -> ((msg, now) :: profanity(nick))

    def isFlooding(nick: Nick) =
        messages(nick).filter{ _ > now-floodTimespan*1000 }.length >= floodThreshold

    def isUsingProfanity(nick: Nick, threshold: Int) =
        profanity(nick).filter{ _._2 > now-profanityTimespan*1000 }.length == threshold

    def isAbusingProfanity(nick: Nick): Boolean =
        isUsingProfanity(nick, profanityThreshold)

    def isProfanity(msg: String) = {
        try {
            val ws = words(msg.replaceAll("[^a-zA-Z0-9 ]+", ""))
            val stmt = ctl.db.prepareStatement("SELECT word FROM irc_profanity WHERE word IN ("+(ws.map(x =>"?").mkString(", "))+") LIMIT 1", ws)
            val results = stmt.executeQuery

            val rowcount = ctl.db.prepareStatement("SELECT FOUND_ROWS() as number").executeQuery.firstRow.getInt("number")

            stmt.close

            rowcount > 0
        } catch {
            case ex: Exception =>
                ctl.db.handleException(ex)
                false
        }
    }

    def cleanup = {
        if (lastCleanup > 100) {
            for (entry <- messages) entry match {
                case (nick, msgs) =>
                    val newMsgs = msgs filter { _ < now-floodTimespan*1000 }
                    if (newMsgs.length == 0) {
                        messages -= nick
                    } else {
                        messages += nick -> newMsgs
                    }
            }

            for (entry <- profanity) entry match {
                case (nick, msgs) =>
                    val newMsgs = msgs filter{ _._2 < now-profanityTimespan*1000 };
                    if (newMsgs.length == 0) {
                        profanity -= nick
                    } else {
                        profanity += nick -> newMsgs
                    }
            }

            for (entry <- shortMessages) entry match {
                case (nick, msgs) =>
                    val newMsgs = msgs filter{ _._2 < now-shortMessagesTimespan*1000 };
                    if (newMsgs.length == 0) {
                        shortMessages -= nick
                    } else {
                        shortMessages += nick -> newMsgs
                    }
            }

            lastCleanup = 0
        } else {
            lastCleanup += 1
        }
    }


    def mute(prefix: Prefix, duration: Duration, reason: String) = { 
        ctl.banlog.registerMuteSilent(prefix.nick, duration, "Automatic: "+reason)
        ctl.factoids.lookup(reason+"message") match {
          case Some(factoid) =>
            try {
              ctl.p.msg(channel, String.format(factoid, prefix.nick.name, duration.toString))
            } catch {
              case e =>
                ctl.error("Cound not format: "+e.getMessage)
            }
          case None =>
        }
    }

    // Help info
    val commandsHelp = Map(
      "profanity list"    -> (Set(Administrator, Manager), "!profanity list",           "List all words considered as profanity"),
      "profanity add"     -> (Set(Administrator, Manager), "!profanity add <word>",     "Add <word> to the list of profanities"),
      "profanity remove"  -> (Set(Administrator, Manager), "!profanity remove <word>",  "Remove <word> from the list of profanities")
    )
}
