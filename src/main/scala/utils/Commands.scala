package ircbot
package utils

trait Commands {
    val ctl: Control

    def words(str: String): List[String] = words(str, 0)

    def words(str: String, limit: Int): List[String] =
        str.split("[:,. ]", limit).toList


    class DoAndReply[T](body: => Unit) {
        import scala.actors.Actor
        import scala.actors.Actor._

        import InnerProtocol._

        def onReply[T](pf: PartialFunction[Message, Option[T]])(implicit ms: Long = 5000): Option[T] = {

            var res: Option[T] = None;

            val actor = new Actor {

                def act() = {
                    receive {
                        case FutureRead =>
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
                                }

                                ts -= System.currentTimeMillis - tinit
                            }

                            ctl.c ! StopListening

                            sender ! FutureReply
                    }
                }
            }

            actor.start()
            actor !? FutureRead
            res
        }

    }

    def execute(body: => Unit)= {
        new DoAndReply(body)
    }

}
