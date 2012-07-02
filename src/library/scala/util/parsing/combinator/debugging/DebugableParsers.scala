package scala.util.parsing.combinator
package debugging

import scala.util.parsing.input._
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import annotation.migration
import language.implicitConversions





trait LocationAwareParser {

  object Builder {
  
    var z : AndOrZipper  = AndOrZipper.root
    var initialized : Boolean = false
    var nextLeaf : Leaf = Leaf(NoParserLocation,"", Unparsed)
  
    // Add a word such as "blup" to the parse tree
    def word(loc : ParserLocation, which : WordParser) : Unit = {
  
      // Set next
      nextLeaf = Leaf(loc, which.toString, Parsed)
  
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
    def or(k: Int, loc: ParserLocation, which : EnterParserKind) : Unit = {
      // Update the status of the item before (if any)
      //z = updateLeftStatus
  
      // Construct new item
      var item = Branch( AndOrTree.emptyList(k), Leaf(loc, which.toString, Parsing), OrType)
      // Now replace head with new item and enter
      z = z.down.get.replaceWith(item)
  
      Builder.print
      println("")
      println("")
    }
  
    // Replace first element of list with And( Word(), Word(), ... , Word() );
    // k is th enumber of Words
    def and(k: Int, loc: ParserLocation, which : EnterParserKind) : Unit = {
      // Update the status of the item before (if any)
      //z = updateLeftStatus
      println("Name: " + which.toString + ", k: " + k)
      // Construct new And
      var item = Branch( AndOrTree.emptyList(k), Leaf(loc, which.toString, Parsing), AndType)
  
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
      
    // def enterHandler
    // def exitHandler
  
    def assign(newlvl : Int, newloc : ParserLocation, newmsg : String) = {
      lvl = newlvl
      loc = newloc
      msg = newmsg
    }
    //val andNames : List[String] = List("~","phrase")
    
    // TODO: different return type
    private def defaultExitDispatcher(exit: ParseState): Unit = exit match {
      case Failed(_) => set(0, 0, exit)
      case Parsed    => set(0, 0, exit)
    }
  
    // If we recieve a
    // ~ Then change dispatcher to And
    // | Then change dispatcher to Or
    // "failed" or "parsed" then go to failed/parsed dispatcher
    //   else create a Word from the current location and continue with default dispatch
    def default(which : Any) : Unit = { 
      def default0(entry: EnterParserKind): Unit = entry match {
        case AndParser                => set(2, lvl, entry)
        case OrParser                 => set(2, lvl, entry)
        case IgnoredParser(repr)      => println("Ignoring " + repr)
        case w@WordParser(_)          => Builder.word(loc, w)
        case PhraseParser             => Builder.and(1, loc, entry)
      }
      which match {
        case entry: EnterParserKind   => default0(entry)
        case exit: ParseState         => defaultExitDispatcher(exit)
      }
    }
  
    // If we recieve a
    // | and the level is the same, then add one to the count and continue
    // | and the level is different, then build the final 'or', and start a new count
    // ~ then build the final 'or' and start a new count for 'and'
    // "failed" or "parsed" then go to failed/parsed dispatcher
    //   else return to the default dispatch
    def or(k : Int, initlvl : Int)(which : Any) : Unit = {
      def or0(entry: EnterParserKind): Unit = entry match {
        case AndParser                     => Builder.or(k, loc, OrParser); set(2, lvl, entry)
        case OrParser if (lvl == initlvl)  => set(k + 1, lvl, OrParser)
        case OrParser                      => Builder.or(k, loc, OrParser); set(2, lvl, entry)
        case IgnoredParser(repr)           => Builder.or(k, loc, OrParser); set(0, 0, entry)
        case w@WordParser(repr)            => Builder.or(k, loc, OrParser); set(0, 0, entry); Builder.word(loc, w)
      }
      which match {
        case entry: EnterParserKind        => or0(entry)
        case exit: ParseState              => defaultExitDispatcher(exit)
      }
    }

    // If we recieve a
    // ~ and the level is the same, then add one to the count and continue
    // ~ and the level is different, then build the final 'and' and start a new count
    // | then build the final and and start a new count for 'or'
    // "failed" or "parsed" then go to failed/parsed dispatcher
    //   else return to the default dispatch
    def and(k : Int, initlvl : Int)(which : Any) : Unit = {
      def and0(entry: EnterParserKind): Unit = entry match {
        case AndParser if (lvl == initlvl)  => set(k + 1, lvl, AndParser)
        case AndParser                      => Builder.and(k, loc, AndParser); set(2, lvl, entry)        
        case OrParser                       => Builder.and(k, loc, AndParser); set(2, lvl, entry)
        case IgnoredParser(repr)            => Builder.and(k, loc, AndParser); set(0, 0, entry)
        case w@WordParser(repr)             => Builder.and(k, loc, AndParser); set(0, 0, entry); Builder.word(loc, w)
      }
      which match {
        case entry: EnterParserKind         => and0(entry)
        case exit: ParseState               => defaultExitDispatcher(exit)
      }
    }
  
    // If we recieve a
    // "failed" then ignore and continue
    // | then exit and start new count for 'or'
    // ~ then exit and start new count for 'and'
    //   "parsed" then go to parsed dispatcher
    //   else return to the default dispatch
    def failed(errorMsg : String)(which : Any) : Unit = {
      def failed0(entry: EnterParserKind): Unit = entry match {
        case AndParser | OrParser           => Builder.fail(errorMsg); set(2, lvl, entry)
        case IgnoredParser(repr)            => Builder.fail(errorMsg); set(0, 0, entry)
        case w@WordParser(repr)             => Builder.fail(errorMsg); set(0, 0, entry); Builder.word(loc, w)
      }
      which match {
        case entry: EnterParserKind         => failed0(entry)
        case Failed(_)                      => println(errorMsg) // continue as usual
        case e@Parsed                       => Builder.fail(errorMsg); set(0, 0, e)
      }
    }
  
    // If we recieve a
    // "parsed" then ignore and continue
    // | then exit and start new count for 'or'
    // ~ then exit and start new count for 'and'
    //   "failed" then go to failed dispatcher
    //   else return to the default dispatch
    def parsed(which : Any) : Unit = {
      def parsed0(entry: EnterParserKind): Unit = entry match {
        case AndParser | OrParser           => Builder.parse; set(2, lvl, entry)
        case IgnoredParser(repr)            => Builder.parse; set(0, 0, entry)
        case w@WordParser(repr)             => Builder.parse; set(0, 0, entry); Builder.word(loc, w)
      }
      which match {
        case entry: EnterParserKind         => parsed0(entry)
        case e@Failed(_)                    => Builder.parse; set(0,0, e)
        case Parsed                         =>
      }
    }
    
    // Assigns the next dispatch function to the dispatch variable with appropriate parameters    
    def set(n: Int, lvl: Int, which: EnterParserKind): Unit = which match {
      case OrParser  => go = or(n,lvl)(_)
      case AndParser => go = and(n,lvl)(_)
      case _         => go = default(_)
    }
    
    def set(n: Int, lvl: Int, which: ParseState): Unit = which match {
      case Failed(msg)               => { println("setting go to failed with msg: " + msg); go = failed(msg)(_) }
      case Parsed                    => go = parsed(_)
      case _                         => throw new Exception("invalid set")// can't happen?
    }

}

def toParserKind(s: String): EnterParserKind = {
  def ignore : Boolean = s match {
    case s if(s.indexOf("Parser") >= 0)         => true
    case s if(s.indexOf("parser-map-") >= 0)    => true
    case otherwise                              => false
  }

  s match {
    case "|"             => OrParser
    case "~"             => AndParser
    case "phrase"        => PhraseParser
    case other if ignore => IgnoredParser(other)
    case normal          => WordParser(normal)
  }
}



}

trait DebugableParsers extends LocationAwareParser {
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
        Dispatcher.go(toParserKind(name))

        println("[Name] " + name)
        println("Level:\t" + getLevel(location))
        println("[Input] " + in.first)
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
            Dispatcher.go(Parsed)
          case NoSuccess(msg, next) => {
            println("Failed: " + msg)
            Dispatcher.assign(level, location, msg)
            Dispatcher.go(Failed(msg))
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

