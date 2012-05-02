package scala.util.parsing.combinator
package debugging

import scala.util.parsing.input._
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import annotation.migration
import language.implicitConversions

trait DebugableParsers {
  self: Parsers =>
    
  val debug: Boolean = false//(sys.props.getOrElse("parser.combinators.debug", "false") == "true")
  
  trait NoLocationParser[+T] {
    selfP: Parser[T] =>
    val location: ParserLocation = NoParserLocation
  }
  
  trait DebugableParser[+T] {
    selfP: Parser[T] =>
      
    val location: debugging.ParserLocation
    def ps: List[Parser[T]] = List() // TODO must respect the order
      
    
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
    
    def enterRes(): Unit = {
      if (debug && location != NoParserLocation) {
        // main access point for instrumenting the compiler
        // for now just print statements

        println("Try to consume the token")
        println("[Name] " + name)
        println("[Location] " + location.line + ":" + location.offset)
        println("[File] " + location.fileName)
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
  }
}

