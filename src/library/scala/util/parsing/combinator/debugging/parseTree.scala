package scala.util.parsing.combinator
package debugging

Object ParseTree {


  /* A dual tree consisting of an Or tree and an And tree
   * A '|' separates the Or tree
   * A '~', '~>' or '<~' separates the And tree
   *
   * ParseExample = {
   *     "bim" ~ "bam" ~> "bum"
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
   *  And( [ "bim", "bam", "bum" ], p2 ), 
   *  And( [ "trip", "trop" ], p3),
   *  And( [
   *    Or( [
   *      And( [ "Blip", "Blop" ], p6)
   *     ], p5)
   *   ], p4)
   * ], p1)
   *
   * Except that the strings would be an empty Or with a position containing the string
   */

  abstract class Tree

  case class And(elems : List[Or], leaf : Leaf) extends Tree
  case class Or(elems : List[And], leaf : Leaf) extends Tree

  // The data class for the Leaf. For now it just contains the position
  case class Leaf(pos : ParserLocation)

  // A Zipper for the Tree
  case class Zipper(tree: Tree, breadCrumbs: List[Focus])

  // Breadcrumbs for the zipper
  abstract class Focus
  case class Left(past: Tree) extends Focus
  case class Down(leaf : Leaf, rest : List[Tree]) extends Focus

  // Movement for the zipper class
  def moveLeft(from : Zipper) : Option[Zipper] = from match {
    case Zipper( And(e::es, l), f)              = Some( Zipper( And(e, l), Left(e)::fs) )
    case Zipper( Or(e::es, l), f)               = Some( Zipper( Or(e, l), Left(e)::fs) )
    case Zipper( And(Nil, _), _)                = None
    case Zipper( Or(Nil, _), _)                 = None
  }

  def moveDown(from : Zipper) : Option[Zipper] = from match {
    case Zipper( And(e::es, l), f)              = Some( Zipper( e, Down(l, es)) )
    case Zipper( Or(e::es, l), f)               = Some( Zipper( e, Down(l, es)) )
    case Zipper( And(Nil, _), _)                = None
    case Zipper( Or(Nil, _), _)                 = None
  }

  def moveUp(from : Zipper) : Option[Zipper] = from match {
    case Zipper( And(_, _) @ a, Down(l, r)::fs) = Some( Zipper( Or(a::r, l), fs) )
    case Zipper( Or(_, _) @ a, Down(l, r)::fs)  = Some( Zipper( And(a::r, l), fs) )
    case Zipper( And(e::es, l), Left(p)::fs)    = moveUp( Zipper( And(p::e::es), fs) )
    case Zipper( Or(e::es, l), Left(p)::fs)     = moveUp( Zipper( Or(p::e::es), fs) )
    case Zipper( _, Nil)                        = None
  }

  def moveRight(from : Zipper) : Option[Zipper] = from match {
    case Zipper( And(e::es, l), Left(p)::fs)    = Some( Zipper( And(p::ee::es, l), fs))
    case Zipper( Or(e::es, l), Left(p)::fs)     = Some( Zipper( Or(p::ee::es, l), fs))
    case Zipper( _, Down(_,_))                  = None
    case Zipper( _, [])                         = None
  }

}
