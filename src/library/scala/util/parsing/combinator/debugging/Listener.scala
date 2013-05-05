package scala.util.parsing.combinator
package debugging

trait Listener {
  def stepIn(id: Int, name: String, loc: ParserLocation): Option[Notification]
  
  def stepOut(id: Int, success: Boolean, msg: String): Option[Notification]
}
