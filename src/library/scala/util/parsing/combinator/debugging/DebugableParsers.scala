package scala.util.parsing.combinator
package debugging

import scala.util.parsing.input._
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import annotation.migration
import language.implicitConversions

trait DebugableParsers {
  self: Parsers =>
    
  val debug: Boolean = true//(sys.props.getOrElse("parser.combinators.debug", "false") == "true")
  val init : Boolean = true;
  
  trait NoLocationParser[+T] {
    selfP: Parser[T] =>
    val location: ParserLocation = NoParserLocation
  }

  // Actions the user can take
  abstract class Action 
  case class Step extends Action        // Normal debugging step
  case class StepIn extends Action      // Step into a function
  case class StepOut extends Action     // Step out of a function
  case class Quit extends Action        // Terminate the debugger


  object Controller {

    var lastAction : Action = Step

    def input : Action = {
      // Print empty line and Ask for user input
      println("\n>> ")
      Console.readChar match {
        case 'j'    => Step
        case 'l'    => StepIn
        case 'h'    => StepOut
        case 'q'    => Quit
      }
    }

    def init : Unit = {
      // Print a few statements about how to control the debugger
      println("Welcome to the Combinator Parser Debugger")
      println("Controls: j: Step, l: Step in, h: Step out, q: quit")

      // There might be more things to initalize later one
    }

    // Decides what to do based on last action and current parser
    def step(name : String, loc: debugging.ParserLocation) : Unit = lastStep match {
      // Step isn't right, since it would stop at every '|' and not just the ones in the current scope
      case Step     => if (name == "|") lastAction = input else () // If input is '|' in the same scope, then stop
      case StepIn   => () // we need the parents to work for this
      case StepOut  => () // also need the parent
      case Quit     => exit(0) // Should be somewhere else
    }
  
  }
  
  trait DebugableParser[+T] {
    selfP: Parser[T] =>
      
    val location: debugging.ParserLocation
    def ps: List[Parser[T]] = List() // TODO must respect the order
    def ls: List[debugging.ParserLocation] = List()
      
    
    // TODO inline, or macro 
    def enter(): Unit = {
      if (debug && location != NoParserLocation) {

        // main access point for instrumenting the compiler
        // for now just print statements
        println("Enter parser")
        println("[Name] " + name)
      }
    }
    
    def exit(): Unit = {
      if (debug && location != NoParserLocation) {
        // main access point for instrumenting the compiler
        // for now just print statements
        println("Exit parser")
        println("[Name] " + name)
      }
    }
    
    def enterRes(in: Input): Unit = {
      if (debug && location != NoParserLocation) {

        // Simple check to see if we want to do an init message
        if (init) {
          Controller.init();
          init = false;
        }

        // Leave control flow to the controller
        Controller.step(name, location)


        println(in.pos.longString)
        // main access point for instrumenting the compiler
        // for now just print statements

        println("Try to consume token")
        println("[Name] " + name)
        println("[Location] " + location.line + ":" + location.offset)
        println("[File] " + location.fileName)
        println("[Method] " + printMethod(location) + "\n")
      }
    }

    def exitRes[U >: T](res: ParseResult[U]): Unit = {
      if (debug && location != NoParserLocation) {
        // main access point for instrumenting the compiler
        // for now just print statements
        println("Result of consuming the token")
        println("[Name] " + name)
        println("[Location] " + location.line + ":" + location.offset)
        res match {
          case Success(res0, next) =>
            println("Matched: " + res)
          case NoSuccess(msg, next) =>
            println("Failed: " + msg)
        }
        println("---------------")
      }
    }

    private def printMethod(loc : ParserLocation): String = loc.outer match {
      case null     => location.outerMethod
      case method   => location.outerMethod + " > " + printMethod(loc.outer)
    }
    
  }
}

