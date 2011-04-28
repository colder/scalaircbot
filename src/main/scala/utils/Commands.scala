package ircbot
package utils

trait Commands {
    val ctl: Control

    def words(str: String): List[String] = words(str, 0)

    def words(str: String, limit: Int): List[String] =
        str.split("[:,. ]", limit).toList


    def requireAuth(prefix: Prefix, levels: UserLevel*)(body: => Unit) {
        if (isGranted(prefix, levels :_*)) {
            body
        } else {
            ctl.p.msg(prefix.nick, "Permission Denied")
        }
    }

    def isGranted(prefix: Prefix, levels: UserLevel*): Boolean = {
        val user = new User(ctl, prefix)

        levels.exists(_ == user.level)
    }


    abstract class ExWords(val lim: Int) {
        def unapplySeq(msg: String): Option[Seq[String]] = {
            Some(words(msg, lim))
        }
    }

    object Words  extends ExWords(0)
    object Words2 extends ExWords(2)
    object Words3 extends ExWords(3)
    object Words4 extends ExWords(4)

    class DoAndReply[T](body: => Unit) {
        import InnerProtocol._

        def onReply[T](pf: PartialFunction[Message, Option[T]])(implicit ms: Long = 5000): Option[T] = {
            import scala.actors.Actor._
            import scala.actors.TIMEOUT

            var res: Option[T] = None

            ctl.c ! StartListening

            body

            var continue = true
            var ts = ms

            while(continue && ts > 0) {
                val tinit = System.currentTimeMillis

                ctl.c ! ReadLine
                receiveWithin(ts) {
                    case ReadLineAnswer(line) =>
                        val msg = ctl.p.parseLine(line)

                        if (pf isDefinedAt msg) {
                            pf(msg) match {
                                case Some(r) =>
                                    res = Some(r)
                                    continue = false
                                case None =>
                            }
                        }
		    case TIMEOUT =>
		    	continue = false
                }

                ts -= System.currentTimeMillis - tinit
            }

            ctl.c ! StopListening
            res
        }

    }

    def execute(body: => Unit)= {
        new DoAndReply(body)
    }

}
