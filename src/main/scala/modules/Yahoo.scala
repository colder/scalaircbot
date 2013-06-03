package ircbot
package modules

import helpers.Http
import utils._

class Yahoo(val ctl: Control) extends Module(ctl) with Commands with Http {
    def handleMessage(msg: Message) = msg match {
        case Msg(from, to, msg) if msg startsWith "!web " => {

            if (isGranted(from, Regular, Manager, Administrator)) {
                val dest = to match {
                    case Channel(name) => to
                    case _ => from.nick
                }

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
            val pattern_enc = java.net.URLEncoder.encode(pattern, "UTF-8");
            val res = httpRequest("http://boss.yahooapis.com/ysearch/web/v1/"+pattern_enc+"?appid=WD0C1ofIkY21IlNLjmFl6lC.l7cdajU-&format=xml&filter=-porn-hate&count=5&style=raw")
            val xml = scala.xml.XML.load(res)

            val results = (xml \\  "result").map { r => SearchResult((r \ "abstract").text, (r \ "url").text) }.toList

            SearchResults(results, Integer.parseInt((xml \ "resultset_web" \ "@totalhits").text))

        } catch {
            case e: HttpException =>
                SearchResults(Nil, 0)
        }
    }
}
