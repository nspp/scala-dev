package scala.reflect
package internal

trait Clocks extends api.Clocks { self: SymbolTable =>
  
  // counter should be reset on every run
  private var counter = 0

  def newClockTick(): Clock = {
    assert(isClockOn, "attempting to get current time when internal clock is off")
    counter += 1
    new Tick(counter)
  }
  
  def isClockOn = EV.instrumentingOn
  
  def currentClock() = new Tick(counter)
  
  class Tick(val id: Int) extends Clock
}