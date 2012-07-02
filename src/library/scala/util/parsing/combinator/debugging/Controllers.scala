package scala.util.parsing.combinator.debugging

trait Controllers {
  // A controller deciding what to do given the user input and parser
  object Controller {
  
    var initialized : Boolean = false;
    var z : AndOrZipper = AndOrZipper.root
    var path : List[AndOrFocus] = Nil
  
    def input : Unit = {
      // Play initial message if we haven't been initialized
      if (!initialized) initMsg
  
      // Print empty line and ask for user input
      println("\n>> ")
      Console.readChar match {
        case 's'    => step
        case 'h'    => left
        case 'j'    => down
        case 'k'    => right
        case 'l'    => up
        case 'q'    => quit
      }
  
      // Print z
      println(z)
    }
  
    def quit : Unit = {
      exit(0)
    }
  
    // Leave the control flow to the outside parser
    def step : Unit = {
      // Save current path
      path = z.breadCrumbs
  
      // release control flow
    }
  
    // After a step, enter controller
    def enter(zipper : AndOrZipper) : Unit = {
      // Got to root
      z = zipper.topMost
  
      // Rewind the path to get to last position
      for (d <- path) d match {
        case Right(_)     => z = z.right.get
        case Down(_)      => z = z.down.get
      }
      
      // Then call input and loop
      input
    }
  
    def left : Unit = z.left match {
      case None           => input // At leftmost
      case Some(zNew)     => { z = zNew; input }
    }
  
    def down : Unit = z.down match {
      case None           => input // At downmost
      case Some(zNew)     => { z = zNew; input }
    }
  
    def right : Unit = z.right match {
      case None           => input // At rightmost
      case Some(zNew)     => { z = zNew; input }
    }
  
    def up : Unit = z.up match {
      case None           => input // At upmost
      case Some(zNew)     => { z = zNew; input }
    }
  
    def initMsg : Unit = {
      // Print a few statements about how to control the debugger
      if (initialized) {
        println("Welcome to the Combinator Parser Debugger")
        println("Controls: s: Step, h: Go Left, j: Go Down, k: Go Up, l: Go Right, q: Quit")
      }
  
      // Set init to false
      initialized = true;
  
      // There might be more things to initalize later one
    }
  
  }
  
  // Actions the user can take
  abstract class Action 
  case class Step extends Action        // Normal debugging step
  case class StepIn extends Action      // Step into a function
  case class StepOut extends Action     // Step out of a function
  case class Quit extends Action        // Terminate the debugger
}