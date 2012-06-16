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
  def word(loc : ParserLocation) : Unit = {
    // Construct new Item
    println("Constructing a Word")
    var item = Word(Leaf(loc))
    println(item)
    println("")
    // Move to next position and Add word to zipper
    z = z.nextHead.replaceHead(item).get
  }

  // Replace first element of list with Or( Word(), Word(), ... , Word() );
  // k is the number of Words
  def or(k: Int, loc: ParserLocation) : Unit = {
    println("Constructing an Or Tree")
    // Construct new item
    var item = Or( AndOrTree.emptyList(k), Leaf(loc))
    println(item)
    println("")
    // Now replace head with new item and enter
    z = z.nextHead.replaceHead(item).get.down.get
  }

  // Replace first element of list with And( Word(), Word(), ... , Word() );
  // k is th enumber of Words
  def and(k: Int, loc: ParserLocation) : Unit = {
    println("Constructing an And Tree")
    // Construct new item
    var item = And( AndOrTree.emptyList(k), Leaf(loc))
    println(item)
    println("")
    // Now replace head with new item and enter
    z = z.nextHead.replaceHead(item).get.down.get
  }

  // Mostly for debugging
  def print : Unit = println(z.toString)

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
    var dispatch = defaultDispatch(_,_)

    // If we recieve a
    // ~ Then change dispatcher to And
    // | Then change dispatcher to Or
    //   else create a Word from the current location and continue with default dispatch
    def defaultDispatch(name : String, lvl : Int) : Unit = (name, lvl) match {
      case ("|",n)          => setDispatch(2,n, "or")
      case ("~",n)          => setDispatch(2,n, "and")
      case otherwise        => Builder.word(location)
    }

    // If we recieve a
    // | and the level is the same, then add one to the count and continue
    // | and the level is different, then build the final 'or', and start a new count
    // ~ then build the final 'or' and start a new count for 'and'
    //   else return to the default dispatch
    def orDispatch(k : Int, initlvl : Int)(name : String, curlvl : Int) : Unit = (name, curlvl) match {
      case ("|",n) if (n == initlvl)    => setDispatch(k + 1,n, "or")
      case ("|",n)                      => Builder.or(k, location); setDispatch(2, n, "or")
      case ("~",n)                      => Builder.or(k, location); setDispatch(2,n, "and")
      case otherwise                    => Builder.or(k, location); setDispatch(0,0,"default"); Builder.word(location)
    }

    // If we recieve a
    // ~ and the level is the same, then add one to the count and continue
    // ~ and the level is different, then build the final 'and' and start a new count
    // | then build the final and and start a new count for 'or'
    //   else return to the default dispatch
    def andDispatch(k : Int, initlvl : Int)(name : String, curlvl : Int) : Unit = (name, curlvl) match {
      case ("~",n) if (n == initlvl)    => setDispatch(k + 1,n, "and")
      case ("|",n)                      => Builder.and(k, location); setDispatch(2,n, "or")
      case ("~",n)                      => Builder.and(k, location); setDispatch(2,n, "and")
      case otherwise                    => Builder.and(k, location); setDispatch(0,0,"default"); Builder.word(location)
    }

    // Assigns the next dispatch function to the dispatch variable with appropriate parameters
    def setDispatch(n : Int, lvl : Int, which : String) : Unit = which match {
      case "or"         => dispatch = orDispatch(n,lvl)(_,_)
      case "and"        => dispatch = andDispatch(n,lvl)(_,_)
      case otherwise    => dispatch = defaultDispatch(_,_)
    }

    def enterRes(in: Input): Unit = {
      if (debug && location != NoParserLocation) {

        // Redefine name for easier reading
        if (name == "") name = "Undefined"

        // Get level
        var level = getLevel(location)

        // Call the dispatcher with name and level
        dispatch(name, level)

        println("name:\t" + name)
        println("Level:\t" + getLevel(location))
        println("")

        Builder.print
        println("")
        println("")

        // Simple check to see if we want to do an init message
        // Controller.initMsg;

        // Leave control flow to the controller
        // Controller.step(name, location)

        // let the builder build a parseTree
        // Builder.stepEnter(name, location)

        // println(in.pos.longString)
        // main access point for instrumenting the compiler
        // for now just print statements

        // println("Try to consume token")
        // println("[Name] " + name)
        // println("[Location] " + location.line + ":" + location.offset)
        //println("[File] " + location.fileName)
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
          case NoSuccess(msg, next) =>
            println("Failed: " + msg)
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
