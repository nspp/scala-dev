package scala.reflect
package internal
package event

trait Clocks { self: SymbolTable =>
  abstract class Clock {
    def id: Int
    // provide through ordering implicit
    def <(c: Clock) = id < c.id
    def >(c: Clock) = id > c.id
    def ==(c: Clock) = id == c.id
    
    override def toString() = "[time: " + id + "]"
  }
  
  def newClockTick(): Clock
  def currentClock(): Clock
  def isClockOn: Boolean
  
  object NoClock extends Clock {
    def id = -1
    override def toString() = "[no time]"
  }
}
