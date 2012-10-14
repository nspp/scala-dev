package scala.util.parsing.combinator
package debugging

trait ParserLocation {
    val outer: ParserLocation
    val offset: Int
    val line: Int
    val column: Int
    val fileName: String
    val outerMethod: String // String for now
}
  
case class SomeParserLocation(
  val outer: ParserLocation,
  val offset: Int,
  val line: Int,
  val column: Int,
  val fileName: String,
  val outerMethod: String) extends ParserLocation
  
case object NoParserLocation extends ParserLocation {
  val outer = null
  val offset = -1
  val line = -1
  val column = -1
  val fileName = "<none>"
  val outerMethod = "<none>"
}
