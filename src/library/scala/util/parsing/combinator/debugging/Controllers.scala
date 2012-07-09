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

      // Print z
      println(z.toString)

      // Print empty line and ask for user input
      println("\n>> ")
      Console.readChar match {
        case 's'    => step
        case 'h'    => prev
        case 'j'    => next
        case 'k'    => up
        case 'l'    => nextRight
        case 'q'    => quit
        case _      => println("Press q to quit"); input
      }
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
      for (d <- path.reverse) d match {
        case Right(_)     => z = z.right.get
        case Down(_)      => z = z.down.get
      }
      
      // Then call input and loop
      input
    }

    def next : Unit = (z.right, z.down, z.up) match {
      case (None, None, None)                         => input
      case (None, None, Some(zUp))                    => { z = zUp; right; }
      case (None, Some(zD), Some(zUp)) if zD.isWord   => { z = zUp; right; }
      case (Some(zR), Some(zD), _)     if zD.isWord   => { z = zR; input }
      case (Some(zR), None, _)                        => { z = zR; input; }
      case (_, Some(zD), _)                           => { z = zD; input; }
      case _                                          => input
    }



  /*   def next : Unit = z.down match { */
  /*     case None                           => input // At downmost go right instead */
  /*     case Some(zNew) if zNew.isWord      => right */
  /*     case Some(zNew)                     => { z = zNew; input } */
  /*   } */

    def prev : Unit = z.left match {
      case None           => up // At downmost go right instead
      case Some(zNew)     => { z = zNew; input }
    }

    def nextRight : Unit = z.right match {
      case None           => next // at rightmost, see if it's possible to go down instead
      case Some(zNew)     => { z = zNew; input }
    }
    

    def left : Unit = z.left match {
      case None           => input // At leftmost
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
      println("Welcome to the Combinator Parser Debugger")
      println("Controls: s: Step, h: Go Left, j: Go Down, k: Go Up, l: Go Right, q: Quit")

      // Set init to false
      initialized = true;

      // There might be more things to initalize later one
    }
  
  }
  
}
