package scala.util.parsing.combinator
package debugging

import language.experimental.macros
import scala.reflect.makro.Context

object ParserMacros {
  def location(c: Context) = {
    import c.mirror._
    
    val inscope = c.inferImplicitValue(staticClass("scala.util.parsing.combinator.debugging.ParserLocation").asType)
    val outer = Expr[ParserLocation](if (!inscope.isEmpty) inscope else Literal(Constant(null)))

    val elem: Tree = c.enclosingImplicits(0)._2 match {
      case v@Select(qual, name) =>
        v
      case Apply(fun, args)   => fun
    }
    val DefDef(_, methodName, _, _, _, _) = c.enclosingMethod
    val fileName = elem.pos.fileInfo.getName
    val line = elem.pos.line
    val charOffset = elem.pos.point
    c.reify { SomeParserLocation(outer.eval,c.literal(charOffset).eval, c.literal(line).eval,
                                 c.literal(fileName).eval, c.literal(methodName.toString).eval) }
    //c.reify {NoParserLocation}
  }
  
  implicit def parserLocation: SomeParserLocation = macro location
}
