package ircbot

object ConnectionCounter {
  private[this] var _counter = 0;

  def getNext: Int = {
    _counter += 1
    _counter
  }
}
