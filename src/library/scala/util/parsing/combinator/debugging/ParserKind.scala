package scala.util.parsing.combinator.debugging

sealed abstract class ParserKind

sealed abstract class EnterParserKind extends ParserKind

case class OrParser(name : String, loc : ParserLocation) extends EnterParserKind {
  override def toString: String = name
}
case class AndParser(name : String, loc : ParserLocation) extends EnterParserKind {
  override def toString: String = name
}
case class OtherParser(name : String, loc : ParserLocation) extends EnterParserKind {
  override def toString: String = name
}
case class WordParser(s: String, loc : ParserLocation) extends EnterParserKind {
  override def toString: String = s
}
case class IgnoredParser(s: String, loc : ParserLocation) extends EnterParserKind {
  override def toString: String = "ignored-" + s
}

sealed abstract class ExitParserKind extends ParserKind

case class SuccessParser(msg : String) extends ExitParserKind

case class FailureParser(msg : String) extends ExitParserKind
