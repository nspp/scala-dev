package scala.util.parsing.combinator.debugging

sealed abstract class EnterParserKind

case object OrParser extends EnterParserKind {
  override def toString: String = "|"
}
case object AndParser extends EnterParserKind {
  override def toString: String = "~"
}
case object PhraseParser extends EnterParserKind {
  override def toString: String = "phrase"
}
case class WordParser(s: String) extends EnterParserKind {
  override def toString: String = s
}
case class IgnoredParser(s: String) extends EnterParserKind {
  override def toString: String = "ignored-" + s
}