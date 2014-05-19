package ircbot
package modules

import akka.actor._
import org.joda.time.{DateTime, Period}

import utils._
import InnerProtocol._
import Modes._

class OpControl(val ctl: ActorRef) extends Module {
  var isOp     = Map[Channel, DateTime]() // last OP request, for deop/cleanup
  val requests = new CachedMap[Channel, Set[ActorRef]](Period.seconds(30))

  var stayOpFor = Period.minutes(5)

  override def receive = {
    case Connected =>
      isOp = Map()
      requests.empty()

    case From(_, Mode(chan, Plus(O(nick)) :: Nil )) =>
      currentState.map {
        case bs if (bs.nick == nick) =>
          logInfo("Opped in "+chan.name)
          isOp += chan -> now()
          requests.getOrElse(chan, Set()).foreach {
            _ ! chan
          }

        case bs =>
          println("woot "+bs.nick+" "+nick)
      }

    case From(_, Mode(chan, Minus(O(nick)) :: Nil )) =>
      currentState.map {
        case bs if (bs.nick == nick) =>
          logInfo("Deopped from "+chan.name)
          isOp -= chan

        case bs =>
          println("woot "+bs.nick+" "+nick)
      }

    case RequestOp(chan) =>
      if (isOp contains chan) {
        isOp += chan -> now()
        sender ! chan
      } else {
        requests += chan -> (requests.getOrElse(chan, Set()) + sender)
        send(Msg(Nick.ChanServ, s"OP ${chan.name}"))
      }

    case GC =>
      requests.gc()

    case Tick =>
      for ((chan, last) <- isOp if now().isAfter(last.plus(stayOpFor))) {
        currentState.map { s =>
          send(Mode(chan, List(Minus(O(s.nick)))))
        }
      }

    case _ =>
  }
}
