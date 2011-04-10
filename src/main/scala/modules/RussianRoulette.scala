package ircbot.modules

import ircbot._
import helpers.Auth
import helpers.Commands

import scala.util.Random
import scala.collection.mutable.HashMap

class RussianRoulette(ctl: Control) extends Module(ctl) with Auth with Commands {
    val gun = new Random;
    val barils = new HashMap[Nick, Int]();

    def handleMessage(msg: Message) = msg match {
        case Msg(from, to : Channel, msg) =>
            // msg sent on channel
            words(msg, 2) match {
                case "!RR" :: nickname :: Nil =>
                    val nick = Nick(nickname)
                    if (isGranted(ctl, from, Manager, Administrator)) {
                        ctl.p.msg(to, nickname+" has been volunteered to play russian roulette! Roll...");
                        Thread.sleep(2000);
                        if (gun.nextInt(getBaril(nick)) == 0) {
                            ctl.chanserv.doAsOP(to) {
                                ctl.p.kick(to, nick, "Bang!")
                                resetBaril(nick);
                            }
                        } else {
                            useBaril(nick);
                            ctl.p.msg(to, "Click!")
                        }
                    } else {

                        if (getBaril(from.nick) != 3 && gun.nextInt(getBaril(from.nick)) == 0) {
                            ctl.chanserv.doAsOP(to) {
                                ctl.p.kick(to, from.nick, "Told you not to play with guns!")
                                resetBaril(from.nick);
                            }
                        } else {
                            useBaril(from.nick);
                            ctl.p.msg(from.nick, "Don't play with guns, kid!")
                        }
                    }
                    false
                case _ =>
                    true
            }
        case _ => true
    }


    def getBaril(nick: Nick): Int = barils.get(nick) match {
        case Some(count) => count
        case None => 3
    }

    def useBaril(nick: Nick) = barils(nick) = (getBaril(nick)-1);

    def resetBaril(nick: Nick) = barils(nick) = 3;

}
