package scala.util.parsing.combinator
package debugging

object Zipper {

  // Creates the root zipper to start out with
  def root(loc : ParserLocation) : Zipper = Zipper(AndOrTree.empty, [])

}


// Breadcrumbs for the zipper
abstract class AndOrPos
case class Right(past: AndOrTree) extends AndOrPos
case class Down(leaf : Leaf, rest : List[AndOrTree]) extends AndOrPos


// A Zipper for the AndOrTree
case class Zipper(tree: AndOrTree, breadCrumbs: List[Focus]) {

  z =>

  // Movement for the zipper class
  def right : Option[Zipper] = z match {
    case Zipper( And(e::es, l), fs)                 => Some( Zipper( And(es, l), Right(e)::fs) )
    case Zipper( Or(e::es, l), fs)                  => Some( Zipper( Or(es, l), Right(e)::fs) )
    case Zipper( And(Nil, _), _)                    => None
    case Zipper( Or(Nil, _), _)                     => None
  }

  def down : Option[Zipper] = z match {
    case Zipper( And(e::es, l), fs)                 => Some( Zipper( e, Down(l, es)::fs) )
    case Zipper( Or(e::es, l), fs)                  => Some( Zipper( e, Down(l, es)::fs) )
    case Zipper( And(Nil, _), _)                    => None
    case Zipper( Or(Nil, _), _)                     => None
  }

  def left : Option[Zipper] = z match {
    case Zipper( And(es, l), Right(p:Or)::fs)       => Some( Zipper( And(p::es, l), fs))
    case Zipper( Or(es, l), Right(p:And)::fs)       => Some( Zipper( Or(p::es, l), fs))
    case Zipper( _, Down(_,_)::_)                   => None
    case Zipper( _, Nil)                            => None
  }

  def up : Option[Zipper] = z match {
    case Zipper( e:And, Down(l, r:List[And])::fs)   => Some( Zipper( Or(e::r, l), fs) )
    case Zipper( e:Or, Down(l, r:List[Or])::fs)     => Some( Zipper( And(e::r, l), fs) )
    case Zipper( And(es, l), Right(p:Or)::fs)       => Zipper( And(p::es, l), fs) ).up
    case Zipper( Or(es, l), Right(p:And)::fs)       => Zipper( Or(p::es, l), fs) ).up
    case Zipper( _, Nil)                            => None
  }

  def leftMost : Zipper = z.left match {
    case None                                       => z
    case Some(z_new)                                => z_new.leftmost
  }

  // Add an And to an Or tree and moves the zipper to the new position
  def add(elem : AndOrTree) : Zipper = z.add([elem])
    case Zipper( e, fs)                             => Zipper(e, Right(elem)::fs)
  }

  // Add list moving to the end of the list
  // This can be done better with a fold, but I'm not sure how
  def addList(elems : List[AndOrTree]) : Zipper = {
    case Zipper( e, fs)                             => Zipper(e, elems.map(e => Right(e)):::fs)
  }

  def addLoc(at : Zipper)(loc : ParserLocation) : Zipper = add(at)(And(Nil, Leaf(loc)))

}
