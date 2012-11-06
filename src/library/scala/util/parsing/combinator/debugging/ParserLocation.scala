package scala.util.parsing.combinator
package debugging

trait ParserLocation {
    val outer: ParserLocation
    val offset: Int
    val line: Int
    val column: Int
    val fileName: String
    val outerMethod: String // String for now
    def isValid: Boolean // NoParserLocation, cannot just use the object because
                         // of implicit resolution ambiguity problem
}
  
class SomeParserLocation(
  val outer: ParserLocation,
  val offset: Int,
  val line: Int,
  val column: Int,
  val fileName: String,
  val outerMethod: String) extends ParserLocation {
  def isValid = true
}
  
object NoParserLocation extends SomeParserLocation(
  outer = null, 
  offset = -1,
  line = -1,
  column = -1,
  fileName = "<none>",
  outerMethod = "<none>") {
  override def isValid = false
}
