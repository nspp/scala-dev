package scala.util.parsing.combinator
package debugging

trait ParserLocation {
    val outer: ParserLocation
    val offset: Int
    val line: Int
    val fileName: String
    val outerMethod: String // String for now
    val rawLocation: String
}
  
case class SomeParserLocation(
  val outer: ParserLocation,
  val offset: Int,
  val line: Int,
  val fileName: String,
  val outerMethod: String,
  val rawLocation: String) extends ParserLocation
  
case object NoParserLocation extends ParserLocation {
  val outer = null
  val offset = -1
  val line = -1
  val fileName = "<none>"
  val outerMethod = "<none>"
  val rawLocation = "<none>"
}
