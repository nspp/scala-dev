package scala.util.parsing.combinator
package debugging

object AndOrZipper {

  // Creates the root zipper to start out with
  def root(loc : ParserLocation) : AndOrZipper = AndOrZipper(Or(Nil, Leaf(NoParserLocation)), Nil)

}


// Breadcrumbs for the zipper
abstract class AndOrFocus
case class Right(past: AndOrTree) extends AndOrFocus
case class Down(leaf : Leaf, rest : List[AndOrTree]) extends AndOrFocus


// A AndOrZipper for the AndOrTree
case class AndOrZipper(tree: AndOrTree, breadCrumbs: List[AndOrFocus]) {

  z : AndOrZipper =>

  // Movement for the zipper class
  def right : Option[AndOrZipper] = z match {
    case AndOrZipper( And(e::es, l), fs)                 => Some( AndOrZipper( And(es, l), Right(e)::fs) )
    case AndOrZipper( Or(e::es, l), fs)                  => Some( AndOrZipper( Or(es, l), Right(e)::fs) )
    case AndOrZipper( And(Nil, _), _)                    => None
    case AndOrZipper( Or(Nil, _), _)                     => None
  }

  def down : Option[AndOrZipper] = z match {
    case AndOrZipper( And(e::es, l), fs)                 => Some( AndOrZipper( e, Down(l, es)::fs) )
    case AndOrZipper( Or(e::es, l), fs)                  => Some( AndOrZipper( e, Down(l, es)::fs) )
    case AndOrZipper( And(Nil, _), _)                    => None
    case AndOrZipper( Or(Nil, _), _)                     => None
  }

  def left : Option[AndOrZipper] = z match {
    case AndOrZipper( And(es, l), Right(p:Or)::fs)       => Some( AndOrZipper( And(p::es, l), fs))
    case AndOrZipper( Or(es, l), Right(p:And)::fs)       => Some( AndOrZipper( Or(p::es, l), fs))
    case AndOrZipper( _, Down(_,_)::_)                   => None
    case AndOrZipper( _, Nil)                            => None
  }

  def up : Option[AndOrZipper] = z match {
    case AndOrZipper( e:And, Down(l, r:List[And])::fs)   => Some( AndOrZipper( Or(e::r, l), fs) )
    case AndOrZipper( e:Or, Down(l, r:List[Or])::fs)     => Some( AndOrZipper( And(e::r, l), fs) )
    case AndOrZipper( And(es, l), Right(p:Or)::fs)       => AndOrZipper( And(p::es, l), fs).up
    case AndOrZipper( Or(es, l), Right(p:And)::fs)       => AndOrZipper( Or(p::es, l), fs).up
    case AndOrZipper( _, Nil)                            => None
  }

  def leftMost : AndOrZipper = z.left match {
    case None                                       => z
    case Some(z_new)                                => z_new.leftMost
  }

  // Add an And to an Or tree and moves the zipper to the new position
  def add(elem : AndOrTree) : AndOrZipper = z match {
    case AndOrZipper(e, fs)                             => AndOrZipper(e, Right(elem)::fs)
  }

  // Add list moving to the end of the list
  // This can be done better with a fold, but I'm not sure how
  def addList(elems : List[AndOrTree]) : AndOrZipper = z match{
    case AndOrZipper(e, fs)                             => AndOrZipper(e, elems.reverse.map(e => Right(e)):::fs)
  }

  def replaceHead(elem : AndOrTree) : Option[AndOrZipper] = z match {
    case AndOrZipper(And(e::es, l), fs)                 => Some( AndOrZipper(And(elem::es, l), fs) )
    case AndOrZipper(Or(e::es, l), fs)                  => Some( AndOrZipper(Or(elem::es, l), fs) )
    case AndOrZipper(Word(l), fs)                       => AndOrZipper(elem, fs)
    case otherwise                                      => None
  }

  def atEnd : Boolean = z match {
    case AndOrZipper(And(Nil, l), fs)                   => true
    case AndOrZipper(Or(Nil, l), fs)                    => true
    case otherwise                                      => false
  }

  def nextHead : AndOrZipper = (z.atEnd, z.up) match {
    case (true, None)                                   => Error("There is no more elements to fill")
    case (false, _)                                     => z
    case (true, zup)                                    => zup.nextHead
  }


  // Updates the information about the current active node
  def setLeaf(leaf : Leaf) : AndOrZipper = z match {
    case AndOrZipper( And(elems, _), fs)                => AndOrZipper( And(elems, leaf), fs);
    case AndOrZipper( Or(elems, _), fs)                 => AndOrZipper( Or(elems, leaf), fs);
  }

}
