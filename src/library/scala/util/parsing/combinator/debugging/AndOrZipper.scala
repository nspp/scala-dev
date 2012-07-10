package scala.util.parsing.combinator
package debugging

object AndOrZipper {

  // Creates the root zipper to start out with
  def root : AndOrZipper = AndOrZipper(Word(Leaf(NoParserLocation,"root",Unparsed)), Nil)

}


// Breadcrumbs for the zipper
abstract class AndOrFocus
case class Right(past: AndOrTree) extends AndOrFocus
case class Down(past : Branch) extends AndOrFocus


// A AndOrZipper for the AndOrTree
case class AndOrZipper(tree: AndOrTree, breadCrumbs: List[AndOrFocus]) {

  z : AndOrZipper =>

  // Movement for the zipper class
  def right : Option[AndOrZipper] = z match {
    case AndOrZipper( Branch(e::es, l, t), fs)          => Some( AndOrZipper( Branch(es, l, t), Right(e)::fs) )
    case AndOrZipper( Branch(Nil, l, t), fs)            => None
    case AndOrZipper( Word(_), _)                       => None
  }

  def down : Option[AndOrZipper] = z match {
    case AndOrZipper( Branch(e::es, l, t), fs)          => Some( AndOrZipper( e, Down(Branch(es, l, t))::fs) )
    case AndOrZipper( Branch(Nil, l, t), fs)            => None
    case AndOrZipper( Word(_), _)                       => None
  }

  def left : Option[AndOrZipper] = z match {
    case AndOrZipper( Branch(es, l, t), Right(p)::fs)   => Some( AndOrZipper( Branch(p::es, l, t), fs))
    case AndOrZipper( Word(_), _)                       => None
    case AndOrZipper( _, Down(_)::_)                    => None
    case AndOrZipper( _, Nil)                           => None
  }

  def up : Option[AndOrZipper] = z match {
    case AndOrZipper( e, Down(p) :: fs)                 => Some( AndOrZipper( p.insert(e), fs) )
    case AndOrZipper( e:Branch, Right(p) :: fs)         => AndOrZipper( e.insert(p), fs).up
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

  // Checks if we are at the top
  def isTopMost : Boolean = z.up match {
    case None                                           => true
    case _                                              => false
  }

  def replaceHead(f : AndOrTree => AndOrTree) : Option[AndOrZipper] = z match {
    case AndOrZipper(Branch(e::es, l, t), fs)           => Some( AndOrZipper(Branch(f(e)::es, l, t), fs) )
    case AndOrZipper(w @ Word(_), fs)                   => None
  }

  // Replaces the head of the list of trees in the current element
  def replaceHeadWith(elem : AndOrTree) : Option[AndOrZipper] = replaceHead(_ => elem)

  // Modifies current tree with f
  def replace(f : AndOrTree => AndOrTree) : AndOrZipper = z match {
    case AndOrZipper(tree, fs)                          => AndOrZipper(f(tree), fs)
  }

  // Replaces current tree with elem
  def replaceWith(elem : AndOrTree) : AndOrZipper = replace(_ => elem)

  // Returns the next head, even if we need to go up to get it. If there aren't any left we throw an error
  def next : AndOrZipper = (z.atEnd, z.isRoot, z.up) match {
    case (true, false, None)                            => z
    case (true, true, zup)                              => z
    case (true, false, zup)                             => zup.get.next
    case (false, _, _)                                  => z.right.get
  }

  // Updates the information about the current active node
  def changeLeaf(f : Leaf => Leaf) : AndOrZipper = z match {
    case AndOrZipper( Branch(elems, l, t), fs)          => AndOrZipper( Branch(elems, f(l), t), fs);
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
    case AndOrZipper( Branch(_,_, AndType), _)          => true
    case otherwise                                      => false
  }

  // Checks if the current element is an Or
  def isOr : Boolean = z match {
    case AndOrZipper( Branch(_,_, OrType), _)           => true
    case otherwise                                      => false
  }

  def isRoot : Boolean = z.tree match {
    case Word(Leaf(NoParserLocation,"root",Unparsed))   => true
    case otherwise                                      => false
  }

  // Checks if we are at the end of a branch
  def atEnd : Boolean = z.tree match {
    case Branch(e::Nil, l, t)                           => true
    case Word(_)                                        => true
    case otherwise                                      => false
  }


  override def toString : String = {
    z.safeMove(_.down)
     .changeLeaf({case Leaf(pos, name,status) => Leaf(pos, name + "\t <<",status)})
     .topMost.leftMost.tree.toString// + "\n\n" + parserPos
  }

  def parserPos : String = z match {
    case AndOrZipper(Branch(_, Leaf(loc, _, _), _), _)  => getFileDetails(loc)
    case AndOrZipper(Word(Leaf(loc, _, _)),_)          => getFileDetails(loc)
  }

  def getFileDetails(loc : ParserLocation) : String = {
    import scala.io.Source._
    println(new java.io.File(".").getAbsolutePath)
    println(loc.fileName)

    val lines = fromFile(loc.fileName).getLines
    val segment = lines.drop(loc.line-1).take(3)
    return segment.toString
  }


}
