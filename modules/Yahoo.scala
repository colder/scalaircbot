package ircbot.modules

import helpers.Http
import helpers.Auth

class Yahoo(ctl: Control) extends Module(ctl) with Http with Auth {
    def handleMessage(msg: Message) = msg match {
        case Msg(from, to, msg) if msg startsWith "!web " => {

            if (isGranted(ctl, from, Normal, Manager, Administrator)) {
                val dest = if (to startsWith "#") to else from.nick

                val pattern = msg substring 5;
                val res = search(pattern);
                res.results match {
                    case x::xs => 
                        ctl.p.msg(dest, x.url+" - "+x.desc);
                    case Nil =>
                        ctl.p.msg(dest, "No result for \""+pattern+"\"");
                }
            } else {
                ctl.p.msg(from.nick, "This command can only be used by regulars.")
            }
            false
        }
        case _ => true
    }

    case class SearchResult(desc: String, url: String)
    case class SearchResults(results: List[SearchResult], total: Int)

    def search(pattern: String): SearchResults = {
        try {
            val pattern_enc = java.net.URLEncoder.encode(pattern);
            val res = httpRequest("http://boss.yahooapis.com/ysearch/web/v1/"+pattern_enc+"?appid=WD0C1ofIkY21IlNLjmFl6lC.l7cdajU-&format=xml&filter=-porn-hate&count=5&style=raw")
            val xml = scala.xml.XML.load(res)

            val results = (xml \\  "result") map { r => SearchResult((r \ "abstract").text, (r \ "url").text) } toList

            SearchResults(results, Integer.parseInt((xml \ "resultset_web" \ "@totalhits").text))

        } catch {
            case e: HttpException =>
                SearchResults(Nil, 0)
        }
    }
}
