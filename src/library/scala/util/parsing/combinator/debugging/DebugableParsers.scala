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


  object Controller {

    def step[U](res: ParseResult[U]) : Unit = {

      // Check if we should stop. For now we only stop at '|'s

      
      // Check if current location is child of last location
    
    }

    def init() : Unit = {
      // Print a few statements about how to control the debugger
      println("Welcome to the Combinator Parser Debugger")
      println("Controls: j: Step, l: Step in, h: Step out, q: quit")

      // There might be more things to initalize later one
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

