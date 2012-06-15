package scala.util.parsing.combinator
package debugging

object AndOrZipper {

  // Creates the root zipper to start out with
  def root : AndOrZipper = AndOrZipper(Word(Leaf(NoParserLocation)), Nil)

}


// Breadcrumbs for the zipper
abstract class AndOrFocus
case class Right(past: AndOrTree) extends AndOrFocus
case class Down(past : AndOrBranch) extends AndOrFocus


// A AndOrZipper for the AndOrTree
case class AndOrZipper(tree: AndOrTree, breadCrumbs: List[AndOrFocus]) {

  z : AndOrZipper =>

  // Movement for the zipper class
  def right : Option[AndOrZipper] = z match {
    case AndOrZipper( And(e::es, l), fs)                => Some( AndOrZipper( And(es, l), Right(e)::fs) )
    case AndOrZipper( Or(e::es, l), fs)                 => Some( AndOrZipper( Or(es, l), Right(e)::fs) )
    case AndOrZipper( And(Nil, _), _)                   => None
    case AndOrZipper( Or(Nil, _), _)                    => None
    case AndOrZipper( Word(_), _)                       => None
  }

  def down : Option[AndOrZipper] = z match {
    case AndOrZipper( And(e::es, l), fs)                => Some( AndOrZipper( e, Down(And(es, l))::fs) )
    case AndOrZipper( Or(e::es, l), fs)                 => Some( AndOrZipper( e, Down(Or(es, l))::fs) )
    case AndOrZipper( And(Nil, _), _)                   => None
    case AndOrZipper( Or(Nil, _), _)                    => None
    case AndOrZipper( Word(_), _)                       => None
  }

  def left : Option[AndOrZipper] = z match {
    case AndOrZipper( And(es, l), Right(p)::fs)         => Some( AndOrZipper( And(p::es, l), fs))
    case AndOrZipper( Or(es, l), Right(p)::fs)          => Some( AndOrZipper( Or(p::es, l), fs))
    case AndOrZipper( Word(_), _)                       => None
    case AndOrZipper( _, Down(_)::_)                    => None
    case AndOrZipper( _, Nil)                           => None
  }

  def up : Option[AndOrZipper] = z match {
    case AndOrZipper( e, Down(p) :: fs)                 => Some( AndOrZipper( p.insert(e), fs) )
    case AndOrZipper( e:AndOrBranch, Right(p) :: fs)    => AndOrZipper( e.insert(p), fs).up
    case AndOrZipper( Word(_), _)                       => None
    case AndOrZipper( _, Nil)                           => None
  }

  def leftMost : AndOrZipper = z.left match {
    case None                                           => z
    case Some(z_new)                                    => z_new.leftMost
  }

  // Replaces the head of the list of trees in the current element
  def replaceHead(elem : AndOrTree) : Option[AndOrZipper] = z match {
    case AndOrZipper(And(e::es, l), fs)                 => Some( AndOrZipper(And(elem::es, l), fs) )
    case AndOrZipper(Or(e::es, l), fs)                  => Some( AndOrZipper(Or(elem::es, l), fs) )
    case AndOrZipper(Word(_), fs)                       => Some( AndOrZipper(elem, fs) )
    case otherwise                                      => None
  }

  // Returns the next head, even if we need to go up to get it. If there aren't any left we throw an error
  def nextHead : AndOrZipper = (z.atEnd, z.up) match {
    case (true, None)                                   => sys.error("There is no more elements to fill")
    case (true, zup)                                    => zup.get.nextHead
    case (false, _)                                     => z
  }

  // Updates the information about the current active node
  def setLeaf(leaf : Leaf) : AndOrZipper = z match {
    case AndOrZipper( And(elems, _), fs)                => AndOrZipper( And(elems, leaf), fs);
    case AndOrZipper( Or(elems, _), fs)                 => AndOrZipper( Or(elems, leaf), fs);
    case AndOrZipper( Word(l), fs)                      => AndOrZipper( Word(leaf), fs)
  }

  // Checks if the current element is a Word or a branch
  def isWord : Boolean = z match {
    case AndOrZipper( Word(_), _)                       => true
    case otherwise                                      => false
  }

  // Checks if we are at the end of a branch
  def atEnd : Boolean = z match {
    case AndOrZipper(And(Nil, l), fs)                   => true
    case AndOrZipper(Or(Nil, l), fs)                    => true
    case AndOrZipper(Word(_), fs)                       => true
    case otherwise                                      => false
  }

}
