package scala.util.parsing.combinator
package debugging

object AndOrZipper {

  // Creates the root zipper to start out with
  def root : AndOrZipper = AndOrZipper(Word(Leaf(NoParserLocation,"root")), Nil)

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

  def safeMove(move : AndOrZipper => Option[AndOrZipper]) : AndOrZipper = move(z) match {
    case None                                           => z
    case z_new @ Some(_)                                => z_new.get
  }

  // Go as much left as possible
  def leftMost : AndOrZipper = z.left match {
    case None                                           => z
    case Some(z_new)                                    => z_new.leftMost
  }

  // Go as much up as possible
  def topMost : AndOrZipper = z.up match {
    case None                                           => z
    case Some(z_new)                                    => z_new.topMost
  }

  def replaceHead(f : AndOrTree => AndOrTree) : Option[AndOrZipper] = z match {
    case AndOrZipper(And(e::es, l), fs)                 => Some( AndOrZipper(And(f(e)::es, l), fs) )
    case AndOrZipper(Or(e::es, l), fs)                  => Some( AndOrZipper(Or(f(e)::es, l), fs) )
    case AndOrZipper(w @ Word(_), fs)                   => Some( AndOrZipper(f(w), fs) )
    case otherwise                                      => None
  }

  // Replaces the head of the list of trees in the current element
  def replaceHeadWith(elem : AndOrTree) : Option[AndOrZipper] = replaceHead(_ => elem)

  // Returns the next head, even if we need to go up to get it. If there aren't any left we throw an error
  def next : AndOrZipper = (z.atEnd, z.isRoot, z.up) match {
    case (true, false, None)                            => sys.error("There are no more elements to fill")
    case (true, true, zup)                              => z
    case (true, false, zup)                             => zup.get.next
    case (false, _, _)                                  => z.right.get
  }

  // Updates the information about the current active node
  def changeLeaf(f : Leaf => Leaf) : AndOrZipper = z match {
    case AndOrZipper( And(elems, l), fs)                => AndOrZipper( And(elems, f(l)), fs);
    case AndOrZipper( Or(elems, l), fs)                 => AndOrZipper( Or(elems, f(l)), fs);
    case AndOrZipper( Word(l), fs)                      => AndOrZipper( Word(f(l)), fs)
  }

  // Updates the information about the current active node
  def setLeaf(leaf : Leaf) : AndOrZipper = z.changeLeaf(_ => leaf)

  // Checks if the current element is a Word or a branch
  def isWord : Boolean = z match {
    case AndOrZipper( Word(_), _)                       => true
    case otherwise                                      => false
  }

  // Checks if the current element is an And
  def isAnd : Boolean = z match {
    case AndOrZipper( And(_,_), _)                       => true
    case otherwise                                      => false
  }

  // Checks if the current element is an Or
  def isOr : Boolean = z match {
    case AndOrZipper( Or(_,_), _)                       => true
    case otherwise                                      => false
  }

  def isRoot : Boolean = z.tree match {
    case Word(Leaf(NoParserLocation,"root"))            => true
    case otherwise                                      => false
  }

  // Checks if we are at the end of a branch
  def atEnd : Boolean = z.tree match {
    case And(e::Nil, l)                                 => true
    case Or(e::Nil, l)                                  => true
    case Word(_)                                        => true
    case otherwise                                      => false
  }


  override def toString : String = z.safeMove(_.down).changeLeaf({case Leaf(pos, name) => Leaf(pos, name + "\t <<")}).topMost.tree.toString

}
