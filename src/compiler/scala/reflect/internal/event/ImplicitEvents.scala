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

    case class ImplicitDone(originEvent: Int, optTree: Tree, origTree: Tree)
      extends TreeEvent with ImplicitEvent with SymbolReferencesEvent with DoneBlock {
      override def tag = super.tag + "-done"
      def tree = origTree
      def coercion = optTree
      def references = if (coercion.symbol != NoSymbol) List(coercion.symbol) else List()
      def coercionFound = coercion match {
        case EmptyTree      => false
        case t if t.isEmpty => false
        case _              => true
      }
    }

    case class InferImplicit(tree: Tree, pt: Type, reportAmbiguous: Boolean,
        isView: Boolean, undetParams: List[Symbol], byName: Boolean, e: Explanation)
      extends TreeEvent with TreeReferencesEvent with ImplicitEvent {
      override def tag = super.tag + (if (byName) "-by-name" else "") + "-search"
      def references = if (tree.isEmpty)
        e match {
          case expl: TreeInfo =>
            List(expl.tree)
          case _ => List()
        } else List()
      override protected def participants: List[Any] = List(tree, pt)
    }
    
    // info2 - new implicit
    case class ImprovesCompare(info1: ImplicitInfo, info2: ImplicitInfo)
      extends Event with SymbolReferencesEvent with ImplicitEvent {
      override def tag = super.tag + "-compare-two-implicits"
      def participants = List(info1, info2)
      def references = List(info1.sym, info2.sym)
    }
    
    case class ImprovesResult(info1: ImplicitInfo, info2: ImplicitInfo, originEvent: Int, res: Boolean)
      extends Event with SymbolReferencesEvent with ImplicitEvent with DoneBlock {
      override def tag = super.tag + "-compared-two-implicits"
      def participants = List(info1, info2)
      def references = List(info1.sym, info2.sym)
    }
    

    case class VerifyImplicit(newTree: Tree, oldTree: Tree,
        pt: Type, info: ImplicitInfo)(implicit e: Explanation)
      extends TreeEvent with ImplicitEvent with SymbolReferencesEvent {
      override def tag = "verify-available-implicit"
      def tree = if (oldTree.isEmpty)
        e match {
          case expl: TreeInfo => expl.tree
          case _ => oldTree
        } else oldTree
      def references = List(info.sym)
    }

    case class ImplicitSearchDone(originEvent: Int)
      extends Event with ImplicitEvent with DoneBlock {
      def participants = List()
      override def tag = "implicits-search-done"
    }

    case class DivergentImplicitEvent() extends Event with ImplicitEvent with SoftErrorEvent {
      def participants = List()
      override def tag = "divergent-implicit"
      def errPos = NoPosition
    }

    case class SearchContextImplicits(tree0: Tree, allImplicits: List[List[ImplicitInfo]])(implicit e: Explanation)
      extends TreeEvent with ImplicitEvent {
      override def tag = super.tag + "-context"
      def tree = if (tree0.isEmpty)
        e match {
          case expl: TreeInfo =>
            expl.tree
          case _ => tree0
        } else tree0
    }

    case class ManifestAndExpectedTpeImplicits(tree: Tree, pt: Type)
      extends TreeEvent with ImplicitEvent {
      override def tag = super.tag + "-manifests-and-expected-type"
    }

    case class SearchExpectedTypeImplicits(pt: Type)
      extends Event with ImplicitEvent {
      override def tag = super.tag + "-expected-type"
      def participants = List()
   }

    case class SearchManifestImplicits(tpe: Type)
      extends Event with ImplicitEvent {
      override def tag = super.tag + "-manifest"
      def participants = List()
    }
    
    // advanced
    trait ImplicitsEligibility
    
    case class AllEligibleImplicits(pt: Type)
      extends Event with ImplicitEvent with ImplicitsEligibility {
      override def tag = super.tag + "-eligible-test"
      def participants = List()
    }
    
    case class AllEligibleImplicitsDone(pt: Type, originEvent: Int, infos: List[ImplicitInfo])
      extends Event with ImplicitEvent with DoneBlock with SymbolReferencesEvent with ImplicitsEligibility{
      override def tag = super.tag + "-eligible-test-done"
      def participants = infos
      def references = infos.map(_.sym)
    }
    
    case object CategorizeImplicits extends Event with ImplicitEvent with ImplicitsEligibility {
      override def tag = super.tag + "-outer-delimiter"
      def participants = List()
    }
    
    case class InfoEligibleTest(info: ImplicitInfo)
      extends Event with ImplicitEvent with SymbolReferencesEvent with ImplicitsEligibility {
      override def tag = super.tag + "-eligible-info"
      def participants = List(info.sym)
      def references = List(info.sym)
    }
    
    case class InfoEligibleTestDone(info: ImplicitInfo, originEvent: Int, eligible: Boolean)
      extends Event with ImplicitEvent with DoneBlock with ImplicitsEligibility {
      override def tag = super.tag + "-eligible-info"
      def participants = List(info)
      def references = List(info.sym)
    }
    
    
    case class CheckedTypesCompatibility(tp: Type, pt: Type, fast: Boolean, res: Boolean)
      extends Event with ImplicitEvent {
      override def tag = super.tag + "-checked-types-comp"
      def participants = List(tp, pt)
    }
    // end advanced 

    case class AmbiguousImplicitsError(tree: Tree,
      info1Sym: Symbol, info1Tpe: Type,
      info2Sym: Symbol, info2Tpe: Type)
      extends TreeEvent with ImplicitEvent with SymbolReferencesEvent with SoftErrorEvent {
      override def tag = super.tag + "-ambiguous-implicits"
      override def participants = List(info1Sym, info2Sym)
      def references = List(info1Sym, info2Sym)
      def errPos = tree.pos
    }

    case class PossiblyValidImplicit(originEvent: Int, origTree: Tree, sym: Symbol, tpe: Type, result: Boolean)
      extends TreeEvent with ImplicitEvent with SymbolReferencesEvent with DoneBlock {
      def tree = origTree
      private def prefix = if (result) "Valid" else "Invalid"
      override def tag = super.tag + "-" + prefix.toLowerCase + "-implicit"
      override def participants = List(tree)
      def references = List(sym)
    }

    case class CyclicReferenceInImplicitsImprove(info: ImplicitInfo)
      extends Event with ImplicitEvent with SymbolReferencesEvent with SoftErrorEvent {
      def participants = List(info)
      def references = List(info.sym)
      def errPos = info.sym.pos
    }
  }
}