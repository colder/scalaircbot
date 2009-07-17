package ircbot.modules

import helpers.Auth
import helpers.Commands

import scala.util.Random
import scala.collection.mutable.HashMap

class RussianRoulette(ctl: Control) extends Module(ctl) with Auth with Commands {
    val gun = new Random;
    val barils = new HashMap[String, Int]();

    def handleMessage(msg: Message) = msg match {
        case Msg(from, to, msg) =>
            if (to.toList.head == '#') {
                // msg sent on channel
                words(msg, 2) match {
                    case "!RR" :: nick :: Nil =>
                        if (isGranted(ctl, from, Manager, Administrator)) {
                            ctl.p.msg(to, nick+" has been volunteered to play russian roulette! Roll...");
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
            } else {
                true
            }
        case _ => true
    }


    def getBaril(nick: String): Int = barils.get(nick) match {
        case Some(count) => count
        case None => 3
    }

    def useBaril(nick: String) = barils(nick) = (getBaril(nick)-1);

    def resetBaril(nick: String) = barils(nick) = 3;

}
