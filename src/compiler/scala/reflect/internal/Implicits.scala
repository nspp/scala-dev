package scala.reflect
package internal

trait Implicits {
  self: SymbolTable =>

      /** A class that records an available implicit
   *  @param   name   The name of the implicit
   *  @param   pre    The prefix type of the implicit
   *  @param   sym    The symbol of the implicit
   */
  class ImplicitInfo(val name: Name, val pre: Type, val sym: Symbol) {
    private var tpeCache: Type = null

    /** Computes member type of implicit from prefix `pre` (cached). */
    def tpe: Type = {
      if (tpeCache eq null) tpeCache = pre.memberType(sym)
      tpeCache
    }

    def isCyclicOrErroneous =
      try containsError(tpe)
      catch { case _: CyclicReference => true }

    var useCountArg: Int = 0
    var useCountView: Int = 0

    /** Does type `tp` contain an Error type as parameter or result?
     */
    private def containsError(tp: Type): Boolean = tp match {
      case PolyType(tparams, restpe) =>
        containsError(restpe)
      case NullaryMethodType(restpe) =>
        containsError(restpe)
      case MethodType(params, restpe) =>
        params.exists(_.tpe.isError) || containsError(restpe)
      case _ =>
        tp.isError
    }

    /** Todo reconcile with definition of stability given in Types.scala */
    private def isStable(tp: Type): Boolean = tp match {
     case TypeRef(pre, sym, _) =>
       sym.isPackageClass ||
       sym.isModuleClass && isStable(pre) /*||
       sym.isAliasType && isStable(tp.normalize)*/
     case _ => tp.isStable
    }
    def isStablePrefix = isStable(pre)

    override def equals(other: Any) = other match {
      case that: ImplicitInfo =>
          this.name == that.name &&
          this.pre =:= that.pre &&
          this.sym == that.sym
      case _ => false
    }
    override def hashCode = name.## + pre.## + sym.##
    override def toString = name + ": " + tpe
  }

}