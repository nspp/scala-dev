package scala.util.parsing.combinator
package debugging

import scala.util.parsing.input._
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import annotation.migration
import language.implicitConversions
import debugging.{parseTree, ParserLocation}

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

  var z : ParseTree.Zipper  = ParseTree.root(NoParserLocation)
  var initialized : Boolean = false;
  var level : Int           = 0

  // Initializes to the root position
  def init(loc : ParserLocation) : Unit = {
    if (initialized == false) z = ParseTree.root(loc)
    initialized = true;
  }

  def stepEnter(name : String, loc: ParserLocation) : Unit = (getLevel(loc), name) match {
    case (l, "|") if (l == level)   => next(loc)
    case (l, "|") if (l > level)    => in(loc)
  }

  // Add new element to list of branches
  def next(loc : ParserLocation) : Unit = { z = ParseTree.addLoc(z)(loc) }

  // Add an element to 
  def in(loc : ParserLocation) : Unit = {
    // Add element
    var tempZipper  = ParseTree.moveDown(ParseTree.moveLeft(z).get).get
    z               = ParseTree.addLoc(tempZipper)(loc)
    level           = getLevel(loc)
  }

  // TODO: make into two functions. one that takes success and one that takes failure
  // def stepExit[U](res: ParseResult[U], loc : ParserLocation) : Unit = res match {
  //   case Success(res0, next)        => 
  //   case NoSuccess(msg, next)       => println("Failed: " + msg)
  // 
  // }

  def getLevel(loc : ParserLocation) : Int = loc.outer match {
    case null     => 0
    case method   => getLevel(loc.outer) + 1;
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

        println("name:\t" + name)
        println("Level:\t" + getLevel(location))
        println("")

        // Simple check to see if we want to do an init message
        // Controller.initMsg;

        // Leave control flow to the controller
        // Controller.step(name, location)

        // Simple check to make sure we are initialized
        // Builder.init(loc)

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

