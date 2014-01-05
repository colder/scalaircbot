package ircbot
package utils

import scala.concurrent.duration.FiniteDuration

class CachedMap[A,B](timeout: FiniteDuration) {
  var times = Map[A, Long]()
  var values = Map[A, B]()

  def += (k: A, v: B) {
    values += k -> v
    times += k -> now()
  }

  def += (t: (A,B)) {
    values += t._1 -> t._2
    times += t._1 -> now()
  }

  def -= (k: A) {
    values -= k
    times -= k
  }

  def contains(k: A) = {
    (times contains k)// && !hasExpired(times(k))
  }

  def apply(k: A) = {
    if (contains(k)) {
      values(k)
    } else if(values contains k) {
      throw new NoSuchElementException("key expired: "+k)
    } else {
      throw new NoSuchElementException("key not found: "+k)
    }
  }

  def get(k: A) = {
    if (contains(k)) {
      values.get(k)
    } else {
      None
    }
  }

  def getOrElse(k: A, default: B) = get(k).getOrElse(default)

  def now() = System.currentTimeMillis

  def hasExpired(l: Long) = {
    (now()-l) > timeout.toMillis
  }

  def gc() = {
    for ((k,t) <- times if hasExpired(t)) {
      times -= k
      values -= k
    }
  }

}
