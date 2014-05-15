package ircbot
package modules

import akka.actor._
import org.joda.time.DateTime
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import utils._
import InnerProtocol._

class OpControl(val ctl: ActorRef) extends Module {
  var isOp     = Map[Channel, DateTime]() // last OP request, for deop/cleanup
  val requests = new CachedMap[Channel, Set[ActorRef]](30.seconds)

  var stayOpFor = 5.minutes

  override def receive = {
    case Connected =>
      isOp = Map()
      requests.empty()

    case ReceivedMessage(From(_, Mode(chan, "+o", nick))) =>
      currentState.map {
        case bs if (bs.nick == nick) =>
          logInfo("Opped in "+chan.name)
          isOp += chan -> new DateTime()
          requests.getOrElse(chan, Set()).foreach {
            _ ! chan
          }

        case bs =>
          println("woot "+bs.nick+" "+nick)
      }

    case ReceivedMessage(From(_, Mode(chan, "-o", nick))) =>
      currentState.map {
        case bs if (bs.nick == nick) =>
          logInfo("Deopped from "+chan.name)
          isOp -= chan

        case bs =>
          println("woot "+bs.nick+" "+nick)
      }

    case RequestOp(chan) =>
      if (isOp contains chan) {
        isOp += chan -> new DateTime()
        sender ! chan
      } else {
        requests += chan -> (requests.getOrElse(chan, Set()) + sender)
        send(Msg(Nick.ChanServ, s"OP ${chan.name}"))
      }

    case GC =>
      requests.gc()

    case Tick =>
      for ((chan, last) <- isOp if last.isBefore(new DateTime().minus(stayOpFor.toMillis))) {
        currentState.map { s =>
          send(Mode(chan, "-o", s.nick))
        }
      }

    case _ =>
  }
}
