package scala.reflect
package api

trait Clocks { self: Universe =>
  abstract class Clock {
    def id: Int
    // provide through ordering implicit
    def <(c: Clock) = id < c.id
    def >(c: Clock) = id > c.id
    def ==(c: Clock) = id == c.id
  }
  
  def newClockTick(): Clock
  
  object NoClock extends Clock {
    def id = -1
  }
}