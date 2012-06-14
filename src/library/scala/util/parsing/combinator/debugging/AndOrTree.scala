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
  
  def empty : AndOrTree = Word(Leaf(NoParserLocation))
  def word(loc : ParserLocation) = Word(Leaf(loc))

}

abstract class AndOrTree

case class And(elems : List[Or], leaf : Leaf) extends AndOrTree
case class Or(elems : List[And], leaf : Leaf) extends AndOrTree
case class Word(leaf : Leaf) extends AndOrTree

// The data class for the Leaf. For now it just contains the position
case class Leaf(loc : ParserLocation)
