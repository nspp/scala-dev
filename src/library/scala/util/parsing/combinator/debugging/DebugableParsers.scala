package scala.util.parsing.combinator
package debugging

import scala.util.parsing.input._
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import annotation.migration
import language.implicitConversions

// A controller deciding what to do given the user input and parser
object Controller {

  // Store the user's last action
  var lastAction : Action = Step()
  var initialized : Boolean = true;

  def input : Action = {
    // Print empty line and Ask for user input
    println("\n>> ")
    Console.readChar match {
      case 'j'    => Step()
      case 'l'    => StepIn()
      case 'h'    => StepOut()
      case 'q'    => Quit()
    }
  }

  def initMsg : Unit = {
    // Print a few statements about how to control the debugger
    if (initialized) {
      println("Welcome to the Combinator Parser Debugger")
      println("Controls: j: Step, l: Step in, h: Step out, q: quit")
    }

    // Set init to false
    initialized = false;

    // There might be more things to initalize later one
  }

  // Decides what to do based on last action and current parser
  def step(name : String, loc: ParserLocation) : Unit = lastAction match {
    // Step isn't right, since it would stop at every '|' and not just the ones in the current scope
    case Step()       => if (name == "|") lastAction = input else () // If input is '|' in the same scope, then stop
    case StepIn()     => () // we need the parents to work for this
    case StepOut()    => () // also need the parent
    case Quit()       => exit(0) // Should be somewhere else
  }
}

object Builder {

  var z : AndOrZipper  = AndOrZipper.root
  var initialized : Boolean = false;

  // Add a word such as "blup" to the parse tree
  def word(loc : ParserLocation, name : String) : Unit = {
    // If item before exists and we are in an And, set the status to parsed
    z.left.map(z => 
      if z.isAnd {
        z.replaceHead(e => 
          e.changeLeaf({ case Leaf(loc, name, _) => 
            Leaf(loc, name, Parsed)
          })
        )
      }
    )

    // Construct new Item
    var item = Word(Leaf(loc, name, Unparsed))
    // Move to next position and Add word to zipper
    z = z.replaceHeadWith(item).get.next

    Builder.print
    println("")
    println("")
  }

  // Replace first element of list with Or( Word(), Word(), ... , Word() );
  // k is the number of Words
  def or(k: Int, loc: ParserLocation, name : String) : Unit = {
    // Construct new item
    var item = Or( AndOrTree.emptyList(k), Leaf(loc, name, Unparsed))
    // Now replace head with new item and enter
    z = z.replaceHeadWith(item).get.down.get

    Builder.print
    println("")
    println("")
  }

  // Replace first element of list with And( Word(), Word(), ... , Word() );
  // k is th enumber of Words
  def and(k: Int, loc: ParserLocation, name : String) : Unit = {
    // Construct new item
    var item = And( AndOrTree.emptyList(k), Leaf(loc, name, Unparsed))
    // Now replace head with new item and enter
    z = z.replaceHeadWith(item).get.down.get

    Builder.print
    println("")
    println("")
  }

  // If an element of an And tree failed, we mark it as failed and go up
  def exit(loc: ParserLocation) : Unit = {
    // Mark current position as failed and go up
    z.down.get
     .changeLeaf({ case Leaf(loc, name, state) => Leaf(loc, name, Failed)})
     .up.up
  }

  // Mostly for debugging
  def print : Unit = println(z.toString)

}

object Dispatcher {

  var go = default(_,_,_)
  //val andNames : List[String] = List("~","phrase")

  // If we recieve a
  // ~ Then change dispatcher to And
  // | Then change dispatcher to Or
  //   else create a Word from the current location and continue with default dispatch
  def default(name : String, lvl : Int, loc : ParserLocation) : Unit = (name, lvl) match {
    case ("|",n)                        => set(2, n, "or")
    case ("~",n)                        => set(2, n, "and")
    case ("phrase",n)                   => Builder.and(1, loc, "phrase")
    case ("failed",_)                   => Builder.exit(loc)
    case (s, n) if(ignore(s))           => println("Ignoring " + s)
    case otherwise                      => Builder.word(loc, name)
  }

  // If we recieve a
  // | and the level is the same, then add one to the count and continue
  // | and the level is different, then build the final 'or', and start a new count
  // ~ then build the final 'or' and start a new count for 'and'
  //   else return to the default dispatch
  def or(k : Int, initlvl : Int)(name : String, curlvl : Int, loc : ParserLocation) : Unit = (name, curlvl) match {
    case ("|",n) if (n == initlvl)      => set(k + 1,n, "or")
    case ("|",n)                        => Builder.or(k, loc, "|"); set(2, n, "or")
    case ("~",n)                        => Builder.or(k, loc, "|"); set(2,n, "and")
    case ("failed",_)                   => {} // What to do here?
    case (s, n) if (ignore(s))          => Builder.or(k, loc, "|"); set(0,0,"default");
    case otherwise                      => Builder.or(k, loc, "|"); set(0,0,"default"); Builder.word(loc, name)
  }

  // If we recieve a
  // ~ and the level is the same, then add one to the count and continue
  // ~ and the level is different, then build the final 'and' and start a new count
  // | then build the final and and start a new count for 'or'
  //   else return to the default dispatch
  def and(k : Int, initlvl : Int)(name : String, curlvl : Int, loc : ParserLocation) : Unit = (name, curlvl) match {
    case ("~",n) if (n == initlvl)      => set(k + 1,n, "and")
    case ("|",n)                        => Builder.and(k, loc, "~"); set(2,n, "or")
    case ("~",n)                        => Builder.and(k, loc, "~"); set(2,n, "and")
    case ("failed",_)                   => {} // What to do here?
    case (s, n) if (ignore(s))          => Builder.and(k, loc, "~"); set(0,0,"default");
    case otherwise                      => Builder.and(k, loc, "~"); set(0,0,"default"); Builder.word(loc, name)
  }

  // Assigns the next dispatch function to the dispatch variable with appropriate parameters
  def set(n : Int, lvl : Int, which : String) : Unit = which match {
    case "or"         => go = or(n,lvl)(_,_,_)
    case "and"        => go = and(n,lvl)(_,_,_)
    case otherwise    => go = default(_,_,_)
  }

  def ignore(s : String) : Boolean = s match {
    case s if(s.indexOf("Parser") >= 0)         => true
    case s if(s.indexOf("parser-map-") >= 0)    => true
    case otherwise                              => println(">> " + s + " <<"); false
  }

}


// Actions the user can take
abstract class Action 
case class Step extends Action        // Normal debugging step
case class StepIn extends Action      // Step into a function
case class StepOut extends Action     // Step out of a function
case class Quit extends Action        // Terminate the debugger



trait DebugableParsers {
  self: Parsers =>

  val debug: Boolean = true//(sys.props.getOrElse("parser.combinators.debug", "false") == "true")

  trait NoLocationParser[+T] {
    selfP: Parser[T] =>
    val location: ParserLocation = NoParserLocation
  }


  trait DebugableParser[+T] {
    selfP: Parser[T] =>

    val location: debugging.ParserLocation
    def ps: List[Parser[T]] = List() // TODO must respect the order
    def ls: List[debugging.ParserLocation] = List()


    def enterRes(in: Input): Unit = {
      if (debug && location != NoParserLocation) {

        // Redefine name for easier reading
        if (name == "") name = "Undefined"

        // Get level
        var level = getLevel(location)

        // Call the dispatcher with name and level
        Dispatcher.go(name, level, location)

        println("[Name] " + name)
        println("Level:\t" + getLevel(location))
        println("")

        // println("Try to consume token")
        // println("[Name] " + name)
        // println("[Location] " + location.line + ":" + location.offset)
        // println("[File] " + location.fileName)
        // println("[Method] " + printMethod(location) + "\n")
        // println("")
      }
    }

    def exitRes[U >: T](res: ParseResult[U]): Unit = {
      if (debug && location != NoParserLocation) {
        // main access point for instrumenting the compiler
        // for now just print statements
        // println("Result of consuming the token")
        // println("[Name] " + name)
        // println("[Location] " + location.line + ":" + location.offset)

        // Call builder on exit
        // Builder.stepExit(res, location)

        res match {
          case Success(res0, next) =>
            println("Matched: " + res)
          case NoSuccess(msg, next) => {
            var level = getLevel(location)
            Dispatcher.go("failed", level, location)
            println("Failed: " + msg)
          }
        }
        println("")
      }
    }

    private def printMethod(loc : ParserLocation): String = loc.outer match {
      case null     => loc.outerMethod
      case method   => printMethod(loc.outer) + " > " + loc.outerMethod
    }

    private def getLevel(loc : ParserLocation) : Int = loc.outer match {
      case null     => 0
      case method   => getLevel(loc.outer) + 1;
    }

  }
}

