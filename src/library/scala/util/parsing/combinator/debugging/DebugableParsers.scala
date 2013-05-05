package scala.util.parsing.combinator
package debugging

import scala.util.parsing.input._
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import annotation.migration
import language.implicitConversions

import scala.collection.{mutable, immutable}

trait LocationAwareParser {

  object Builder {

    var z : AndOrZipper  = AndOrZipper.root
    var initialized : Boolean = false
    var nextLeaf : Leaf = Leaf(NoParserLocation,"", Unparsed)

    def getZipper : AndOrZipper = z

    // Add a word such as "blup" to the parse tree
    def word(loc : ParserLocation, which : ParserKind) : Unit = {

      // Set next
      nextLeaf = Leaf(loc, which.toString, Parsed)

      /* Builder.print */
      /* println("(word)") */
      /* println("") */
    }

    def tunnel : Unit = {
      // Construct new stick
      var item = Branch( AndOrTree.emptyList(1), nextLeaf.changeState(Parsing), AndType)
      // Replace head with stick
      z = z.replaceHeadWith(item).get

      // If z is root, just replace current item
      if (z.isRoot) z= z.replaceWith(item)
      // Else, go down and replace
      else z = z.down.get.replaceWith(item)
      // Now replace head with new item and enter
    }

    // Replace first element of list with Or( Word(), Word(), ... , Word() );
    // k is the number of Words
    def or(k: Int, p : OrParser) : Unit = {
      // Update the status of the item before (if any)
      //z = updateLeftStatus

      // Construct new item
      var item = Branch( AndOrTree.emptyList(k), Leaf(p.loc, p.name, Parsing), OrType)
      // Now replace head with new item and enter
      z = z.down.get.replaceWith(item)

      /* Builder.print */
      /* println("(or)") */
      /* println("") */
    }

    // Replace first element of list with And( Word(), Word(), ... , Word() );
    // k is the number of Words
    def and(k: Int, p : AndParser) : Unit = {
      // Update the status of the item before (if any)
      //z = updateLeftStatus
      //println("Name: " + name + ", k: " + k)
      // Construct new And
      var item = Branch( AndOrTree.emptyList(k), Leaf(p.loc, p.name, Parsing), AndType)

      // If z is root, just replace current item
      if (z.isRoot) z= z.replaceWith(item)
      // Else, go down and replace
      else z = z.down.get.replaceWith(item)
      // Now replace head with new item and enter

      /* Builder.print */
      /* println("(and)") */
      /* println("") */
    }

    // If an element of an And tree failed, we mark it as failed and go up
    def fail(msg : String) : Unit = {
      // Mark current position as failed and go up
      var failedWord = Word(nextLeaf.changeState(Failed(msg)))

      // Update tree
      z = z.replaceHeadWith(failedWord).get

      // Make sure parents are appropriately failed
      failParent(msg)

      // move to next
      z = z.next

      println(z.toString)
      println("")
      println("")
    }

    // In case a word has failed parsing, check if we should fail the parent too
    def failParent(msg : String) : Unit = {
      // If we are in an And tree ...
      if (z.isAnd || (z.isOr && z.atEnd)) {
        // Change leaf and move up
        z = z.changeLeaf({ case Leaf(l,n,_) => Leaf(l,n,Failed(msg)) })
        z.up match {
          case Some(zip)  => z = zip; failParent(msg)
          case None       => {}
        }
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
        // Change leaf
        z = z.changeLeaf({ case Leaf(l,n,_) => Leaf(l,n,Parsed) })
        // and move up
        z.up match {
          case Some(zip)  => z = zip; parseParent
          case None       => {}
        }
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
    var level = 0

    def default(which : ParserKind) : Unit = which match { 
      case IgnoredParser(repr,_)                => println("Ignoring " + repr)
      case OtherParser(name,loc)                => Builder.and(1, AndParser(name, loc))
      case _                                    => set(which)
    }

    def word(l : Int)(which : ParserKind) : Unit = which match { 
      case WordParser(_,loc) if l != level      => Builder.tunnel; set(which)
      case _                                    => set(which)
    }

    def or(k : Int, d : Int, first : OrParser)(which : ParserKind) : Unit = which match {
      case OrParser(_,loc) if depth(loc) == d   => go = or(k+1, d, first)_
      case _                                    => Builder.or(k, first); set(which)
    }

    def and(k : Int, d : Int, first : AndParser)(which : ParserKind) : Unit = which match {
      case AndParser(_,loc) if depth(loc) == d  => go = and(k+1, d, first)_
      case _                                    => Builder.and(k, first); set(which)
    }
  
    def failed(which : ParserKind) : Unit = which match {
      case FailureParser(_)                     => {} // Continue as usual
      case _                                    => set(which)
    }

    def parsed(which : ParserKind) : Unit = which match {
      case SuccessParser(_)                     => {} // Continue as usual
      case _                                    => set(which)
    }

    // Assigns the next dispatch function to the dispatch variable with appropriate parameters    
    def set(which: ParserKind): Unit = which match {
      case OrParser(name, loc)                  => go = or(2, depth(loc), OrParser(name, loc))_
      case AndParser(name, loc)                 => go = and(2, depth(loc), AndParser(name, loc))_
      case FailureParser(msg)                   => go = failed(_); Builder.fail(msg);
      case SuccessParser(_)                     => go = parsed(_); Builder.parse;
      case WordParser(_,loc)                    => go = word(level)_; Builder.word(loc, which)
      case _                                    => go = default(_)
    }

    def incLevel : Unit = { level = level + 1 }
    def decLevel : Unit = { level = level - 1 }
    def getLevel : Int = level

    def depth(loc : ParserLocation) : Int = loc.outer match {
      case null     => 0
      case method   => depth(method) + 1
    }
  }


  def toParserKind(s: String, loc : ParserLocation): ParserKind = {
    def ignore : Boolean = s match {
      case s if(s.indexOf("Parser") >= 0)         => true
      case s if(s.indexOf("parser-map-") >= 0)    => true
      case s if(s != "" && s.head == '`')         => true
      case otherwise                              => false
    }
    s match {
      case "|" | "|||"                  => OrParser(s, loc)
      case "~" | "~>" | "<~" | "~!"     => AndParser(s, loc)
      case "phrase"                     => OtherParser(s, loc)
      case other if ignore              => IgnoredParser(other, loc)
      case normal                       => WordParser(normal, loc)
    }
  }
}




trait DebugableParsers extends DefaultParser with LocationAwareParser with Controllers {
  self: Parsers =>

  var contr : Controller = null

  private var listeners: List[Listener] = Nil

  def addListener(elem: Listener) = {
    this.synchronized {
      listeners = elem :: listeners
    }
  }
  
  def clearListeners() = {
    this.synchronized {
      listeners = Nil
    }
  }

  private[this] var ids = 0

  object DebuggableParserFactory extends ParserFactory {
    def apply[T](f: Input => ParseResult[T]): Parser[T] = new DebugableParser[T] {
      def consume(in: Input) = f(in)
      val location: debugging.ParserLocation = debugging.NoParserLocation
      def ps = Nil
    }
    def apply[T](f: Input => ParseResult[T], loc0: debugging.ParserLocation): Parser[T]
      = new DebugableParser[T] {
          def consume(in: Input) = f(in)
          val location: debugging.ParserLocation = loc0
          def ps = Nil
        }
    def apply[T](f: Input => ParseResult[T], loc0: debugging.ParserLocation, ps0: => List[Parser[T]]): Parser[T]
      = new DebugableParser[T] {
          def consume(in: Input) = f(in)
          val location: debugging.ParserLocation = loc0
          def ps: List[Parser[T]] = ps0
        }
    }

  abstract override val mkParser: ParserFactory = DebuggableParserFactory

  abstract override def phrase[T](p: Parser[T])(implicit loc: debugging.ParserLocation): Parser[T] = new DebugableParser[T] {
    def consume(in: Input) = lastNoSuccessVar.withValue(None) {
      p(in) match {
      case s @ Success(out, in1) =>
        if (in1.atEnd)
          s
        else
            lastNoSuccessVar.value filterNot { _.next.pos < in1.pos } getOrElse Failure("end of input expected", in1)
        case ns => lastNoSuccessVar.value.getOrElse(ns)
      }
    }
    
    val location: debugging.ParserLocation = loc
    def ps = Nil
  }.named("phrase") // should be substituted by the 'real' name of the parser location

  abstract override def OnceParser[T](f: Input => ParseResult[T])(implicit loc: debugging.ParserLocation): Parser[T] with OnceParser[T]
    = new DebugableParser[T] with OnceParser[T] {def consume(in: Input) = f(in); val location = loc; def ps = Nil }


  def registerController(c : Controller) : Unit = {
    contr = c
  }

  trait DebugableParser[+T] extends DefaultParser[T] {

    val id = {
      ids += 1
      ids
    }

    val location: debugging.ParserLocation
    def ps: List[Parser[T]] // TODO must respect the order
    def ls: List[debugging.ParserLocation] = List()

    override def apply(in: Input): ParseResult[T] = {
      enterRes(in)
      val res = consume(in)
      exitRes(res)
      res
    }


    def enterRes(in: Input): Unit = {
      if (location != NoParserLocation) {
        // Redefine name for easier reading
        if (name == "") name = "Undefined"

        // Get parserKind
        val p : ParserKind = toParserKind(name, location)

        // Call the dispatcher with name and level
        Dispatcher.incLevel
        Dispatcher.go(p)

        listeners.synchronized {
          val notifiers = listeners.flatMap(_.stepIn(id, name, location))
          waitForAllAcks(notifiers)
        }
      } else {
        if (java.lang.Boolean.getBoolean("parsec.debug"))
          println("NoParserLocation with name: " + name)
      }
    }

    def exitRes[U >: T](res: ParseResult[U]): Unit = {
      if (location != NoParserLocation) {
        Dispatcher.decLevel
        res match {
          case Success(res0, next) =>
            println("Matched: " + res)
            Dispatcher.go(SuccessParser(res0.toString))
          case NoSuccess(msg, next) => {
            println("Failed: " + msg)
            Dispatcher.go(FailureParser(msg))
          }
        }
        println("Level: \t" + Dispatcher.getLevel)

        var msg = res match {
          case Error(m,_) => m
          case Failure(m,_) => m
          case Success(_,_) => "OK"
          case _ => "ERROR"
        }
        listeners.synchronized {
          //val notifiers = listeners.flatMap(_.stepOut(id, res, msg))
          val notifiers = listeners.flatMap(_.stepOut(id, res.successful, msg))
          waitForAllAcks(notifiers)
        }
      }
    }

    def step : Unit = {
      contr.synchronized {
        // Stop the controller

        contr.request.synchronized {
          // Once we resume, fill in some information
          contr.request.field = Builder.z
          // Notify the request that it has been filled
          contr.request.notify
        }
        while (contr.request.field == Builder.z) contr.wait
      }
    }

    def endStep : Unit = {
      contr.synchronized {
        // Stop the controller

        contr.request.synchronized {
          // Once we resume, fill in some information
          contr.request.field = Builder.z
          contr.request.isDone = true
          // Notify the request that it has been filled
          contr.request.notify
        }
      }
    }

    // blocking operation
    private def waitForAllAcks(acks: List[Notification]): Unit = {
      acks foreach (ack => ack.synchronized { 
        while (!ack.ready) ack.wait()
      })
    }

    private def printMethod(loc : ParserLocation): String = loc.outer match {
      case null     => loc.outerMethod
      case method   => printMethod(loc.outer) + " > " + loc.outerMethod
    }


  }
}

