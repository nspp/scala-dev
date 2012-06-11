package scala.util.parsing.combinator
package debugging

object ParseTree {


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

  abstract class Tree

  case class And(elems : List[Or], leaf : Leaf) extends Tree
  case class Or(elems : List[And], leaf : Leaf) extends Tree

  // The data class for the Leaf. For now it just contains the position
  case class Leaf(loc : ParserLocation)

  // A Zipper for the Tree
  case class Zipper(tree: Tree, breadCrumbs: List[Focus])

  // Breadcrumbs for the zipper
  abstract class Focus
  case class Right(past: Tree) extends Focus
  case class Down(leaf : Leaf, rest : List[Tree]) extends Focus

  // The abstract class for directions
  abstract class Direction
  case object L extends Direction
  case object R extends Direction
  case object D extends Direction
  case object U extends Direction

  // Creates the root zipper to start out with
  def root(loc : ParserLocation) : Zipper = Zipper(Or(Nil, Leaf(loc)), Nil)

  // Move by specifying a direction
  def move(from : Zipper)(dir : Direction) : Option[Zipper] = dir match {
    case L      => moveLeft(from);
    case R      => moveRight(from);
    case U      => moveUp(from);
    case D      => moveDown(from);
  }

  // Movement for the zipper class
  def moveRight(from : Zipper) : Option[Zipper] = from match {
    case Zipper( And(e::es, l), fs)                 => Some( Zipper( And(es, l), Right(e)::fs) )
    case Zipper( Or(e::es, l), fs)                  => Some( Zipper( Or(es, l), Right(e)::fs) )
    case Zipper( And(Nil, _), _)                    => None
    case Zipper( Or(Nil, _), _)                     => None
  }

  def moveDown(from : Zipper) : Option[Zipper] = from match {
    case Zipper( And(e::es, l), fs)                 => Some( Zipper( e, Down(l, es)::fs) )
    case Zipper( Or(e::es, l), fs)                  => Some( Zipper( e, Down(l, es)::fs) )
    case Zipper( And(Nil, _), _)                    => None
    case Zipper( Or(Nil, _), _)                     => None
  }

  def moveLeft(from : Zipper) : Option[Zipper] = from match {
    case Zipper( And(es, l), Right(p:Or)::fs)       => Some( Zipper( And(p::es, l), fs))
    case Zipper( Or(es, l), Right(p:And)::fs)       => Some( Zipper( Or(p::es, l), fs))
    case Zipper( _, Down(_,_)::_)                   => None
    case Zipper( _, Nil)                            => None
  }

  def moveUp(from : Zipper) : Option[Zipper] = from match {
    case Zipper( e:And, Down(l, r:List[And])::fs)   => Some( Zipper( Or(e::r, l), fs) )
    case Zipper( e:Or, Down(l, r:List[Or])::fs)     => Some( Zipper( And(e::r, l), fs) )
    case Zipper( And(es, l), Right(p:Or)::fs)       => moveUp( Zipper( And(p::es, l), fs) )
    case Zipper( Or(es, l), Right(p:And)::fs)       => moveUp( Zipper( Or(p::es, l), fs) )
    case Zipper( _, Nil)                            => None
  }

  // Add an And to an Or tree and moves the zipper to the new position
  def add(at : Zipper)(elem : Tree) : Zipper = at match {
    case Zipper( e, fs)                             => Zipper(e, Right(elem)::fs)
  }

  def addLoc(at : Zipper)(loc : ParserLocation) : Zipper = add(at)(And(Nil, Leaf(loc)))

}
