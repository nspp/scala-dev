package scala.util.parsing.combinator.debugging

sealed abstract class ParseState

case class Failed(msg : String) extends ParseState {
  override def toString : String = "Parsing failed: \"" + msg + "\""
}
case object Unparsed extends ParseState {
  override def toString : String = "Not Reached"
}

case object Parsed extends ParseState {
  override def toString: String = "Parsing succeded"
}
case object Parsing extends ParseState {
  override def toString: String = "Parsing"
}
