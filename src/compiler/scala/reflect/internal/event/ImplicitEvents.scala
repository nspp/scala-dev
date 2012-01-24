package scala.reflect
package internal
package event

trait ImplicitEventsUniverse {
  out: SymbolTable with EventsUniverse =>

  trait ImplicitEvents {
    self: EventModel =>

    import Util._

    trait ImplicitEvent {
      def tag = "implicits"
    }

    case class ImplicitDone(originEvent: Int, optTree0: Tree, origTree0: Tree)
      extends TreeEvent with ImplicitEvent with SymbolReferencesEvent with DoneBlock {
      override def tag = super.tag + "-done"
      val tree = duplicateTreeWithPos(origTree0)
      val coercion = duplicateTreeWithPos(optTree0)
      def references = if (coercion.symbol != NoSymbol) List(coercion.symbol) else List()
    }

    case class InferImplicit(tree0: Tree, pt: Type, reportAmbiguous: Boolean,
        isView: Boolean, undetParams: List[Symbol], byName: Boolean, e: Explanation)
      extends TreeEvent with TreeReferencesEvent with ImplicitEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = super.tag + (if (byName) "-by-name" else "") + "-search"
      def references = if (tree0.isEmpty)
        e match {
          case expl: TreeInfo =>
            List(expl.tree)
          case _ => List()
        } else List()
      override protected def participants: List[Any] = List(tree, pt)
    }

    case class VerifyImplicit(newTree0: Tree, oldTree: Tree,
        pt: Type, implicitSym: Symbol, implicitRep: String)(implicit e: Explanation)
      extends TreeEvent with ImplicitEvent with SymbolReferencesEvent {
      override def tag = "verify-available-implicit"
      val tree = duplicateTreeWithPos(if (oldTree.isEmpty)
        e match {
          case expl: TreeInfo => expl.tree
          case _ => oldTree
        } else oldTree)
      val newTree = duplicateTreeWithPos(newTree0)
      def references = List(implicitSym)
    }

    case class ImplicitSearchDone(originEvent: Int)
      extends Event with ImplicitEvent with DoneBlock {
      def participants = List()
      override def tag = "implicits-search-done"
    }

    case class DivergentImplicitEvent() extends Event with ImplicitEvent with SoftErrorEvent {
      def participants = List()
      override def tag = "divergent-implicit"
    }

    case class SearchContextImplicits(tree0: Tree, allImplicits: List[List[ImplicitInfo]])(implicit e: Explanation)
      extends TreeEvent with ImplicitEvent {
      override def tag = super.tag + "-context"
      val tree = duplicateTreeWithPos(if (tree0.isEmpty)
        e match {
          case expl: TreeInfo =>
            expl.tree
          case _ => tree0
        } else tree0)
    }

    case class ManifestAndExpectedTpeImplicits(tree0: Tree, pt0: Type)
      extends TreeEvent with ImplicitEvent {
      val tree = duplicateTreeWithPos(tree0)
      def pt = pt0
      override def tag = super.tag + "-manifests-and-expected-type"
    }

    case class SearchExpectedTypeImplicits(pt0: Type)
      extends Event with ImplicitEvent {
      def pt = pt0
      override def tag = super.tag + "-expected-type"
      def participants = List()
   }

    case class SearchManifestImplicits(tpe0: Type)
      extends Event with ImplicitEvent {
      override def tag = super.tag + "-manifest"
      def tpe = tpe0
      def participants = List()
    }

    case class AmbiguousImplicitsError(tree0: Tree,
      info01Sym: Symbol, info01Tpe: Type,
      info02Sym: Symbol, info02Tpe: Type)
      extends TreeEvent with ImplicitEvent with SymbolReferencesEvent with SoftErrorEvent{
      val tree = duplicateTreeWithPos(tree0)
      val info1Sym = deepSymClone(info01Sym)
      val info1Tpe = deepTypeClone(info01Tpe)
      val info2Sym = deepSymClone(info02Sym)
      val info2Tpe = deepTypeClone(info02Tpe)
      override def tag = super.tag + "-ambiguous-implicits"
      override def participants = List(info1Sym, info2Sym)
      def references = List(info1Sym, info2Sym)
    }

    case class PossiblyValidImplicit(originEvent: Int, origTree0: Tree, sym0: Symbol, tpe0: Type, result: Boolean)
      extends TreeEvent with ImplicitEvent with SymbolReferencesEvent with DoneBlock {
      val tree = duplicateTreeWithPos(origTree0)
      val sym = deepSymClone(sym0)
      def tpe = tpe0
      private def prefix = if (result) "Valid" else "Invalid"
      override def tag = super.tag + "-" + prefix.toLowerCase + "-implicit"
      override def participants = List(tree)
      def references = List(sym)
    }

    case class CyclicReferenceInImplicitsImprove(info: ImplicitInfo)
      extends Event with ImplicitEvent with SymbolReferencesEvent with SoftErrorEvent {
      def participants = List(info)
      def references = List(info.sym)
    }
  }
}