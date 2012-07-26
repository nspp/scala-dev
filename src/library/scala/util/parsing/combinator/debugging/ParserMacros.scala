package scala.util.parsing.combinator
package debugging

import language.experimental.macros
import scala.reflect.makro.Context

object ParserMacros {
  def location(c: Context) = {
    import c.universe._
    
    val inscope = c.inferImplicitValue(c.mirror.staticClass("scala.util.parsing.combinator.debugging.ParserLocation").asType)
    val outer = c.Expr[ParserLocation](if (!inscope.isEmpty) inscope else Literal(Constant(null)))

    val elem: Tree = c.enclosingImplicits(0)._2 match {
      case v@Select(qual, name) =>
        v
      case Apply(fun, args)   => fun
    }
    
    val rawTree: String = "[" + elem.toString + "||" + elem.getClass + "]"
    val DefDef(_, methodName, _, _, _, _) = c.enclosingMethod
    val fileName = elem.pos.fileInfo.getName
    val line = elem.pos.line
    val charOffset = elem.pos.point
    val column = elem.pos.column
    c.reify { SomeParserLocation(outer.splice, c.literal(charOffset).splice, c.literal(line).splice, c.literal(column).splice,
                                 c.literal(fileName).splice, c.literal(methodName.toString).splice, c.literal(rawTree).splice) }
    //c.reify {NoParserLocation}
  }
  
  implicit def parserLocation: SomeParserLocation = macro location
}
