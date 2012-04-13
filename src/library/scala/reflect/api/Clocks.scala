package scala.reflect
package api

trait Clocks { self: Universe =>
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