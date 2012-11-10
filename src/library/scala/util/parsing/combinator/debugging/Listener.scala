package scala.util.parsing.combinator
package debugging

trait Listener {
  // todo: tree will be removed, as it will be built
  // on the fly on the client
  def stepIn(id: Int, name: String, loc: ParserLocation, tree: AndOrZipper): Option[Notification]

  // todo: last will be redundant if we manage 
  // to track that info correctly on the client side
  def stepOut(id: Int, success: Boolean, last: Boolean): Option[Notification]
}