package scala.util.parsing.combinator
package debugging

/* A dual tree consisting of an Or tree and an And tree
 * A '|' separates the elements of the Or tree
 * A '~', '~>' or '<~' separates the elements of the And tree
 *
 * ParseExample = {
 *     "bim" ~ ("bam" ~> "bum")
 *   | "trip" <~ "trop"
 *   | OtherParser
 * }
 *
 * OtherParser = {
 *    "Blip"
 *  | "Blop"
 * }
 *
 * Would be
 *
 * Or( [ 
 *  And( [ "bim", 
      Or( [ 
        And( [ "bam", "bum" ], p4 )
      ], p3)
    ], p2)
 *  And( [ "trip", "trop" ], p5),
 *  And( [
 *    Or( [
 *      And( [ "Blip", "Blop" ], p8)
 *     ], p7)
 *   ], p6)
 * ], p1)
 *
 * Except that the strings would be an empty Or with a position containing the string
 */

object AndOrTree {
  
  def empty : Word = Word(Leaf(NoParserLocation,"",Unparsed))
  def emptyList(n : Int)() : List[Word] = (1 to n).map(i => empty).toList

}

abstract class ParseState

case object Parsed extends ParseState
//case object Parsing extends ParseState
case class Failed(msg : String) extends ParseState {
  override def toString : String = "Failed: \"" + msg + "\""
}
case object Unparsed extends ParseState {
  override def toString : String = "Not Reached"
}

// The whole tree
abstract class AndOrTree {
  override def toString : String = print("")
  // Pretty Print for debugging use
  def print(indent : String) : String
}

// Super class for all branches
// abstract class AndOrBranch extends AndOrTree {
// 
//   // A method for inserting a branch in front of all other branches
//   def insert(e : AndOrTree) : AndOrBranch
// 
//   // A method for returning the first branch
//   def head : Option[AndOrTree]
// 
//   // A method for dropping the first branch
//   def drop : AndOrBranch
// }

abstract class AndOrType
case object AndType extends AndOrType
case object OrType extends AndOrType

case class Branch(elems : List[AndOrTree], leaf : Leaf, t : AndOrType) extends AndOrTree {
  def insert(e : AndOrTree) : Branch = Branch(e::elems, leaf, t)
  def head : Option[AndOrTree] = elems.headOption
  def drop : Branch = Branch(elems.drop(1), leaf, t)

  // Pretty Print for debugging use
  def print(indent : String) : String = {
    var s : String = indent + " " + t + ": " + leaf + "\n"
    elems.foreach{e => s = s + e.print(indent + "  ") + "\n"}
    s
  }
}

// The And class, used for elements separated by a ~
// case class And(elems : List[AndOrTree], leaf : Leaf) extends AndOrBranch {
//   override def insert(e : AndOrTree) : AndOrBranch = And(e::elems, leaf)
//   override def head : Option[AndOrTree] = elems.headOption
//   override def drop : AndOrBranch = And(elems.drop(1), leaf)
// 
//   // Pretty Print for debugging use
//   def print(indent : String) : String = {
//     var s : String = indent + "And: " + leaf.name + "\n"
//     elems.foreach{e => s = s + e.print(indent + "  ") + "\n"}
//     s
//   }
// }
// 
// // The Or class, used for elements separated by a |
// case class Or(elems : List[AndOrTree], leaf : Leaf) extends AndOrBranch {
//   override def insert(e : AndOrTree) : AndOrBranch = Or(e::elems, leaf)
//   override def head : Option[AndOrTree] = elems.headOption
//   override def drop : AndOrBranch = Or(elems.drop(1), leaf)
// 
//   // Pretty Print for debugging use
//   def print(indent : String) : String = {
//     var s : String = indent + "Or: " + leaf.name + "\n"
//     elems.foreach{ e => (s = s + e.print(indent + "  ") + "\n") }
//     s
//   }
// }

case class Word(leaf : Leaf) extends AndOrTree {
  // Pretty Print for debugging use
  def print(indent : String) : String = indent + "Word: "  + leaf
}

// The data class for the Leaf.
case class Leaf(loc : ParserLocation, name : String, state : ParseState) {
  override def toString : String = name + " (" + state + ")"
  def changeState(s  :ParseState) : Leaf = Leaf(loc, name, s)
}
