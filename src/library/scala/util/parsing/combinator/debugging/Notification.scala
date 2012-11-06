package scala.util.parsing.combinator
package debugging

class Notification {
  var ready = false

  def isReady = this.synchronized {
    ready
  }

  def setReady() = this.synchronized {
    ready = true
    notify()
  }
}