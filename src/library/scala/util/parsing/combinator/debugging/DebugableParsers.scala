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
  var initialized : Boolean = false
  var nextLeaf : Leaf = Leaf(NoParserLocation,"", Unparsed)

  // Add a word such as "blup" to the parse tree
  def word(loc : ParserLocation, name : String) : Unit = {

    // Set next
    nextLeaf = Leaf(loc, name, Parsed)

    //z = z.replaceHeadWith(item).get.next
    // Update the status of the item before (if any)
    //// z = updateLeftStatus

    //// Construct new Item
    //var item = Word(Leaf(loc, name, Unparsed))
    //// Move to next position and Add word to zipper

    //Builder.print
    //println("")
    //println("")
  }

  // Replace first element of list with Or( Word(), Word(), ... , Word() );
  // k is the number of Words
  def or(k: Int, loc: ParserLocation, name : String) : Unit = {
    // Update the status of the item before (if any)
    //z = updateLeftStatus

    // Construct new item
    var item = Branch( AndOrTree.emptyList(k), Leaf(loc, name, Parsing), OrType)
    // Now replace head with new item and enter
    z = z.down.get.replaceWith(item)

    Builder.print
    println("")
    println("")
  }

  // Replace first element of list with And( Word(), Word(), ... , Word() );
  // k is th enumber of Words
  def and(k: Int, loc: ParserLocation, name : String) : Unit = {
    // Update the status of the item before (if any)
    //z = updateLeftStatus
    println("Name: " + name + ", k: " + k)
    // Construct new And
    var item = Branch( AndOrTree.emptyList(k), Leaf(loc, name, Parsing), AndType)

    // If z is root, just replace current item
    if (z.isRoot) z= z.replaceWith(item)
    // Else, go down and replace
    else z = z.down.get.replaceWith(item)
    // Now replace head with new item and enter

    Builder.print
    println("")
    println("")
  }

  // If an element of an And tree failed, we mark it as failed and go up
  def fail(msg : String) : Unit = {
    // Mark current position as failed and go up
    var failedWord = Word(nextLeaf.changeState(Failed(msg)))

    // Update tree
    z = z.replaceHeadWith(failedWord).get

    // Make sure parents are appropriately failed
    failParent(msg)

    // If we aren't moving up, move to next
    z = z.right.get

    println(z.toString)
    println("")
    println("")
  }

  // In case a word has failed parsing, check if we should fail the parent too
  def failParent(msg : String) : Unit = {
    // If we are in an And tree ...
    if (z.isAnd || (z.isOr && z.atEnd)) {
      // Change leaf and move up
      z = z.changeLeaf({ case Leaf(l,n,_) => Leaf(l,n,Failed(msg)) }).up.get
      // The fail recursively
      failParent(msg);
    }
  }



  // If an element of a tree was parsed we add it to the tree
  def parse : Unit = {
    // Replace head with word
    z = z.replaceHeadWith(Word(nextLeaf)).get

    // Parse parents
    parseParent

    // Then step to next
    z = z.next

    println(z.toString)
    println("")
    println("")
  }

  // In case a word was parsed, check if we should parse the parent too
  def parseParent : Unit = {

    // If we are in an Or tree or and And tree at last leaf, we change it to parsed and step up
    if (z.isOr || (z.isAnd && z.atEnd)) {
      // Change leaf and move up
      z = z.changeLeaf({ case Leaf(l,n,_) => Leaf(l,n,Parsed) }).up.get
      // Parse parents recusirvely
      parseParent
    }
  }

  // Mostly for debugging
  def print : Unit = println(z.toString)

  // Whenever we parse a new word, we can deduce if the last word was parsed or failed
  // def updateLeftStatus : AndOrZipper = z.left match {
  //   case Some(zip @ AndOrZipper( And(e::es,_),_))   => zip.down.get.changeLeaf({ case Leaf(l, n, _) => Leaf(l, n, Parsed)}).up.get.right.get
  //   case Some(zip @ AndOrZipper( Or(e::es,_),_))    => zip.down.get.changeLeaf({ case Leaf(l, n, _) => Leaf(l, n, Failed("TODO: SET REASON"))}).up.get.right.get
  //   case Some(zip @ AndOrZipper( Word(_),_))        => sys.error("How can we take the left of a word?")
  //   case otherwise                                  => z
  // }


}

object Dispatcher {

  var go = default(_)
  var lvl : Int = 0
  var loc : ParserLocation = NoParserLocation
  var msg : String = ""


  def assign(newlvl : Int, newloc : ParserLocation, newmsg : String) = {
    lvl = newlvl
    loc = newloc
    println("MSG set to: " + newmsg)
    if (newmsg != "") msg = newmsg
  }
  //val andNames : List[String] = List("~","phrase")

  // If we recieve a
  // ~ Then change dispatcher to And
  // | Then change dispatcher to Or
  // "failed" or "parsed" then go to failed/parsed dispatcher
  //   else create a Word from the current location and continue with default dispatch
  def default(name : String) : Unit = (name, lvl) match {
    case ("|",n)                        => set(2, n, "or")
    case ("~",n)                        => set(2, n, "and")
    case ("phrase",n)                   => Builder.and(1, loc, "phrase")
    case ("failed",_)                   => set(0, 0, "failed")
    case ("parsed",_)                   => set(0, 0, "parsed")
    case (s, n) if(ignore(s))           => println("Ignoring " + s)
    case otherwise                      => Builder.word(loc, name)
  }

  // If we recieve a
  // | and the level is the same, then add one to the count and continue
  // | and the level is different, then build the final 'or', and start a new count
  // ~ then build the final 'or' and start a new count for 'and'
  // "failed" or "parsed" then go to failed/parsed dispatcher
  //   else return to the default dispatch
  def or(k : Int, initlvl : Int)(name : String) : Unit = (name, lvl) match {
    case ("|",n) if (n == initlvl)      => set(k + 1,n, "or")
    case ("|",n)                        => Builder.or(k, loc, "|"); set(2, n, "or")
    case ("~",n)                        => Builder.or(k, loc, "|"); set(2,n, "and")
    case ("failed",_)                   => set(0,0,"failed")
    case ("parsed",_)                   => set(0, 0, "parsed")
    case (s, n) if (ignore(s))          => Builder.or(k, loc, "|"); set(0,0,"default");
    case otherwise                      => Builder.or(k, loc, "|"); set(0,0,"default"); Builder.word(loc, name)
  }

  // If we recieve a
  // ~ and the level is the same, then add one to the count and continue
  // ~ and the level is different, then build the final 'and' and start a new count
  // | then build the final and and start a new count for 'or'
  // "failed" or "parsed" then go to failed/parsed dispatcher
  //   else return to the default dispatch
  def and(k : Int, initlvl : Int)(name : String) : Unit = (name, lvl) match {
    case ("~",n) if (n == initlvl)      => set(k + 1,n, "and")
    case ("|",n)                        => Builder.and(k, loc, "~"); set(2,n, "or")
    case ("~",n)                        => Builder.and(k, loc, "~"); set(2,n, "and")
    case ("failed",_)                   => set(0,0,"failed")
    case ("parsed",_)                   => set(0, 0, "parsed")
    case (s, n) if (ignore(s))          => Builder.and(k, loc, "~"); set(0,0,"default");
    case otherwise                      => Builder.and(k, loc, "~"); set(0,0,"default"); Builder.word(loc, name)
  }

  // If we recieve a
  // "failed" then ignore and continue
  // | then exit and start new count for 'or'
  // ~ then exit and start new count for 'and'
  //   "parsed" then go to parsed dispatcher
  //   else return to the default dispatch
  def failed(name : String) : Unit = (name, lvl) match {
    case ("failed",_)                   => {} // continue as usual
    case ("parsed",_)                   => Builder.fail(msg); set(0,0, "parsed")
    case ("|",n)                        => Builder.fail(msg); set(2,n, "or")
    case ("~",n)                        => Builder.fail(msg); set(2,n, "and")
    case (s, n) if (ignore(s))          => Builder.fail(msg); set(0,0,"default");
    case otherwise                      => Builder.fail(msg); set(0,0,"default"); Builder.word(loc, name)
  }

  // If we recieve a
  // "parsed" then ignore and continue
  // | then exit and start new count for 'or'
  // ~ then exit and start new count for 'and'
  //   "failed" then go to failed dispatcher
  //   else return to the default dispatch
  def parsed(name : String) : Unit = (name, lvl) match {
    case ("parsed",_)                   => {} // continue as usual
    case ("failed",_)                   => Builder.parse; set(0,0, "failed")
    case ("|",n)                        => Builder.parse; set(2,n, "or")
    case ("~",n)                        => Builder.parse; set(2,n, "and")
    case (s, n) if (ignore(s))          => Builder.parse; set(0,0,"default");
    case otherwise                      => Builder.parse; set(0,0,"default"); Builder.word(loc, name)
  }

  // Assigns the next dispatch function to the dispatch variable with appropriate parameters
  def set(n : Int, lvl : Int, which : String) : Unit = which match {
    case "or"         => go = or(n,lvl)(_)
    case "and"        => go = and(n,lvl)(_)
    case "failed"     => go = failed(_)
    case "parsed"     => go = parsed(_)
    case otherwise    => go = default(_)
  }

  def ignore(s : String) : Boolean = s match {
    case s if(s.indexOf("Parser") >= 0)         => true
    case s if(s.indexOf("parser-map-") >= 0)    => true
    case otherwise                              => false
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
        Dispatcher.assign(level, location, "")
        Dispatcher.go(name)

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

        var level = getLevel(location)

        res match {
          case Success(res0, next) =>
            println("Matched: " + res)
            Dispatcher.assign(level, location, "")
            Dispatcher.go("parsed")
          case NoSuccess(msg, next) => {
            println("Failed: " + msg)
            Dispatcher.assign(level, location, msg)
            Dispatcher.go("failed")
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

