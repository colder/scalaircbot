package ircbot
package utils

import InnerProtocol._
import scala.concurrent.util.duration._
import scala.concurrent.util.{Duration => ScalaDuration}
import scala.concurrent.Future
import akka.util.Timeout
import akka.actor._
import akka.pattern.ask
import scala.reflect.ClassTag


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

  class DoAndReply(body: => Unit, timeout: ScalaDuration = 20 seconds) {
    class ListeningActor[T : ClassTag](pf: PartialFunction[Message, Option[T]]) extends Actor {
      override def preStart = {
        ctl.c ! StartListening
      }

      var futureSender: ActorRef = null

      def receive = {
        case ReadLine(line) =>
          val msg = ctl.p.parseLine(line)

          if (pf isDefinedAt msg) {
            pf(msg) match {
              case Some(r) =>
                if (futureSender ne null) {
                  futureSender ! r
                  ctl.getContext.stop(self)
                }
              case None =>
            }
          }
        case Start =>
          futureSender = sender
      }

      override def postStop() {
        super.postStop()
        ctl.c ! StopListening
      }
    }

    def onReply[T : ClassTag](pf: PartialFunction[Message, Option[T]]): Future[T] = {
      val la = ctl.getContext.actorOf(Props(new ListeningActor(pf)))
      val f = ask(la, Start)(Timeout(timeout)).mapTo[T]
      body
      f
    }

    def waitUntilReply[T : ClassTag](pf: PartialFunction[Message, Option[T]]): Option[T] = {
      import scala.concurrent.Await
      import scala.concurrent.util.duration._

      implicit val ec = ctl.getContext.dispatcher

      val f = onReply(pf) map {
        x =>
          Some(x)
      } recover {
        case x: Throwable =>
          None
      }

      try {
        Await.result(f, timeout)
      } catch {
        case e: Throwable =>
          ctl.l.err("execute { .. } onReply { .. } timeout!")
          None
      }
    }
  }

  def execute(body: => Unit)= {
    new DoAndReply(body)
  }

}
