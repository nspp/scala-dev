package scala.reflect
package internal
package event

trait InferEventsUniverse {
  out: SymbolTable with EventsUniverse =>

  trait InferEvents {
    self: EventModel =>

    import Util._

    trait InferEvent {
      def tag = "infer"
    }

    trait InferMethodEvent extends InferEvent {
      override def tag = "-method"
    }

    // TODO: status gives false negatives
    case class InferDone(originEvent: Int, status0: Boolean = true)
      extends Event with InferEvent with DoneBlock {
      override def tag = super.tag + "-done"
      def participants = List()
      override def status = status0
    }

    case class InferInstanceDone(originEvent: Int, tree: Tree)
      extends TreeEvent with InferEvent with DoneBlock {
      override def tag = super.tag + "-done"
    }

    case class MethodInfer(fun: Tree, tparams: List[Symbol], formals: List[Type], argtypes: List[Type])
      extends TreeEvent with InferMethodEvent {
      def tree = fun
    }

    case class AdjustTypeArgs(tparams: List[Symbol], argTypes: List[Type], resTpe: Type)
      extends Event with InferEvent {
      override def tag = super.tag + "-adjustTpeArgs"
      def participants = List(tparams, argTypes, resTpe)
    }

    case class CompatibleTypes(found: Type, pt: Type, tparams: List[Symbol])
      extends Event with InferEvent with SymbolReferencesEvent {
      override def tag = "compatible-types"
      def participants = List(found, pt)
      def references = tparams
    }

    // ----

    case class InstantiateTypeVars(tvars: List[TypeVar], tparams: List[Symbol])
      extends Event with InferEvent with SymbolReferencesEvent {
      override def tag = super.tag + "-instantiate-tvars"
      def participants = tvars
      def references = tparams  // TODO replace with originLocation
    }

    case class SolveSingleTVar(tvar: TypeVar, tparam: Symbol, variance: Boolean)
      extends Event with InferEvent with SymbolReferencesEvent {
      override def tag = super.tag +"-solve-tvar"
      def participants = List(tvar, tparam)
      def references = List(tparam)
    }

    case class AddBoundTypeVar(tvar: TypeVar, bound: Type, numeric: Boolean, upperBound: Boolean)
      extends Event with InferEvent {
      def boundType = if (upperBound) "upper" else "lower"
      override def tag = super.tag + "-add-" + boundType + "-bound"
      def participants = List(tvar, bound) // TODO references tvar.typeSymbol
    }

    object TVarSetInst extends Enumeration {
      val ValidType, CovariantPos, ContravariantPos, UpperSubLower, Solve, Relatable = Value
    }

    case class SetInstantiateTypeConstraint(tvar: TypeVar, tp: Type, reason: TVarSetInst.Value)
      extends Event with InferEvent {
      override def tag = super.tag + "-set-instantiate-typeconstr"
      def participants = List(tvar, reason)
    }
    
    case class InstantiateTypeVar(tvar: TypeVar) extends Event with InferEvent {
      override def tag = super.tag + "-instantiate-typevar"
      def participants = List(tvar)
    }
    
    case class WildcardLenientTArg(tvar: TypeVar, noInstance: Boolean) extends Event with InferEvent {
      override def tag = super.tag + "-wildcard-prototype-arg"
      def participants = List(tvar)
    }
    
    case class IncompatibleResultAndPrototype(restpe: Type, pt: Type)
      extends Event with InferEvent {
      override def tag = super.tag + "-incompatible-restpe"
      def participants = List(restpe)
    }

    case class InstantiateGlbOrLub(tvar: TypeVar, up: Boolean, depth: Int)
      extends Event with InferEvent {
      override def tag = super.tag + "-calculate-glborlub-for-tvar-inst"
      def participants = List(tvar)
    }

    case class InstantiateGlbOrLubDone(up: Boolean, originEvent: Int, tp: Type)
      extends Event with InferEvent with DoneBlock {
      override def tag = super.tag + "-calculated-glborlub"
      def participants = List(tp)
    }
    // -----

    case class InferExprInstance(tree: Tree, tparams: List[Symbol], pt: Type)
      extends TreeEvent with InferEvent {
      override def tag = super.tag + "-expr-instance"
    }

    case class InferMethodInstance(fun: Tree, undetparams: List[Symbol], args: List[Tree], pt: Type)
      extends TreeEvent with InferEvent with TreeReferencesEvent {
      def tree = fun
      override def tag = "infer-method-instance-using-inferred-arguments"
      def references = args
    }

    case class InferredMethodInstance(fn: Tree, args: List[Tree])
      extends TreeEvent with InferEvent {
      def tree = fn
      override def tag = "infer-method-instance"
    }

    case class NoInstanceForMethodTypeParameters(fn: Tree, args: List[Tree], exMsg: String)
      extends TreeEvent with InferEvent with SoftErrorEvent {
      def tree = fn
      override def tag = "no-type-parameter-instance-to-infer-method"
    }

    case class MethTypeArgsSolve(tparams: List[Symbol])
      extends Event with InferEvent {
      def participants = List()
    }

    case class SolvedTypes(solvedtypes: List[Type])
      extends Event with  InferEvent {
      def participants = List()
    }

    case class InferMethodInstanceTypeError(fn: Tree, args: List[Tree], exMsg: String)
      extends TreeEvent with InferEvent with SoftErrorEvent {
      def tree = fn
      override def tag = "type-error-instance-to-infer-method"
    }

    case class FailedTypeCompatibilityInInferExprTypeArgs(tpe: Type, pt: Type)
      extends Event with InferEvent with SoftErrorEvent {
      override def tag = "infer-fail-type-compatibility"
      def participants = List()
    }

    case class SubstituteTParamsInfer(tparams: List[Symbol], targs: List[Type], adjust: Boolean)
      extends Event with InferEvent {
      override def tag = super.tag + "-substitute-type-params"
      def participants = List()
    }

    case class PolyTypeInstantiationError(tree: Tree,
      tparams: List[Symbol], polytype: Type, pt: Type)
      extends TreeEvent with InferEvent with HardErrorEvent {
      override def tag = "polymorphic-instantiation-error"
    }

    case class TreeTypeSubstitution(undet: List[Symbol], targs: List[Type], adjusted: Boolean, tree: Tree)
      extends TreeEvent with InferEvent {
      override def tag = "type-substitution-for-inferred-instance"
    }

    case class IsApplicableInfer(undet: List[Symbol], ftpe: Type, pt: Type)
      extends Event with InferEvent {
      override def tag = "is-function-applicable-to-arguments-and-expected-type"
      override def lName = "Is function applicable to\n arguments and expected-type"
      def participants = List()
    }

    case class IsApplicableFallbackInfer(undet: List[Symbol], ftpe: Type, pt: Type)
      extends Event with InferEvent {
      override def tag = "is-function-applicable-to-arguments:fallback-with-pt-?"
      def participants = List()
    }

    case class AdjustedTypeArgs(tparams: List[Symbol],
      argTpes: List[Type], restpe: Type)
      extends Event with InferEvent with DebugEvent {
      override def tag = super.tag + "-adjust-type-arguments"
      def participants = List()
    }

    case class InferMethodAlternative(fun: Tree, argsTpes: List[Type],
        tparams: List[Symbol], pt: Type, alternatives: List[Symbol], implicits: Boolean)
      extends TreeEvent with InferEvent with SymbolReferencesEvent {
      def tree = fun
      override def tag = super.tag + "-method-with-alternatives"
      def references = alternatives
    }

    case class NoBestAlternativeTryWithWildcard(competing: List[Symbol],
        applicable: List[Symbol], pt: Type, tree: Tree)
      extends TreeEvent with InferEvent {
      override def tag = "no-single-best-implicit-found:try-without-pt"
    }

    case class AmbiguousAlternatives(applicable: List[Symbol],
        competing: List[Symbol], best: Symbol, tree: Tree)
      extends TreeEvent with InferEvent with HardErrorEvent with SymbolReferencesEvent {
      override def tag = "ambiguous-alternatives-for-method"
      def references = best::competing
    }

    case class VerifyMethodAlternative(alternative: Symbol, funTpe: Type,
      argsTypes: List[Type], pt: Type, tree: Tree)
      extends TreeEvent with InferEvent with SymbolReferencesEvent {
      override def tag = "verify-method-alternative"
      def references = List(alternative)
    }

    case class PossiblyValidAlternative(overloaded: Tree,
      alternative: Symbol, originEvent: Int, result: Boolean)
      extends TreeEvent with InferEvent with SymbolReferencesEvent with DoneBlock {
      def tree = overloaded
      override def tag = super.tag + "-valid-alternatives"
      override def participants = List(alternative)
      def references = List(alternative)
    }

    case class MethodTypeApplicableDebug(tpe: Type)
      extends Event with InferEvent with DebugEvent {
      override def tag = super.tag + "-method-applicable-debug"
      def participants = List(tpe)
    }

    case class FastTrackMethodTypeApplicableDebug(tpe: Type, undet: List[Symbol])
      extends Event with InferEvent with DebugEvent {
      override def tag = super.tag + "-fast-track-method-applicable-debug"
      def participants = List(tpe)
    }

    case class PolyTypeApplicableDebug(restpe: Type, undet: List[Symbol])
      extends Event with InferEvent with DebugEvent {
      override def tag = super.tag + "-polytype-applicable-debug"
      def participants = List(restpe)
    }

    case class TypesCompatibleDebug(funTpe: Type, argtpes: List[Type], rest: Type)
      extends Event with InferEvent with DebugEvent {
      override def tag = super.tag + "-types-compatible"
      def participants = List(funTpe)
    }

    case class InferExprAlternative(expr: Tree, pt: Type, implicits: Boolean)
      extends TreeEvent with InferEvent {
      def tree = expr
      override def tag = super.tag + "-expr-with-alternatives"
    }

    case class ImprovesAlternativesCheck(alt1: Symbol, alt2: Symbol, expr: Tree)
      extends TreeEvent with InferEvent with SymbolReferencesEvent {
      def tree = expr
      override def tag = "check-conflicting-alternatives"
      def references = List(alt1, alt2)
    }

    case class AlternativesCheck(origEvent: Int, check: Boolean, alt1: Symbol, alt2: Symbol)
      extends Event with InferEvent with SymbolReferencesEvent {
      override def tag = "compared-alternatives"
      def references = List(alt1, alt2)
      def participants = List(alt1, alt2)
    }
  }
  
  trait LubGlbEvents {
    self: EventModel =>

    import Util._
    
    trait LubEvent {
      def tag = "lub"
    }
    
    case class LubGlbDone(originEvent: Int) extends Event with LubEvent with DoneBlock {
      def participants = List()
    }
    
    object LubKindEntry extends Enumeration {
      val Empty, SingleElem, NonTrivial = Value
    }
    
    case class CalcLub(tps: List[Type], kind: LubKindEntry.Value) extends Event with LubEvent {
      override def tag = kind.toString() + "-lub"
      def participants = tps
    }
    
    object LubKindElimSubtypes extends Enumeration {
      val Empty, SingleElem, PolyTpe, MethodTpe, NullaryMethodTpe, TpeBounds, NonTrivial = Value 
    }
    
    case class CalcLubElimSubTypes(tps: List[Type], kind: LubKindElimSubtypes.Value)
      extends Event with LubEvent with SymbolReferencesEvent {
      override def tag = kind.toString() + "-lub-after-elim-subtypes"
      def participants = tps
      def references = tps map (_.typeSymbol)
    }
    
  }

  trait InferExplanations {
    self: EventModel =>

      trait ImplicitsInfo

      trait InferExplanation

      case object WithImplicits extends Explanation with ImplicitsInfo with InferExplanation {
        def descr = "Infer with implicits"
        override def toString = "with implicits"
      }

      case object WithoutImplicits extends Explanation with ImplicitsInfo with InferExplanation {
        def descr = "Infer without implicits"
        override def toString = "without implicits"
      }

      case class InferMethodTypeArguments(argType: Type, formal: Type)
        extends Explanation with InferExplanation {
        def descr = "Infer method type arguments"
        override def toString = "infer-view-for-arguments-compatibility"
      }

      case class InferFunctionTypeViewForArgument(restpe: Type, pt: Type)
        extends Explanation with InferExplanation {
        def descr = "Infer view for result type of the function\n that conforms to the argument type"
        override def toString = "infer-view-for-function-result-type"
      }

      // TODO: move to implicits
      case class TypeImplicitApplication(info: String,
        originalTree: Tree, implicitSym: Symbol)
        extends Explanation with InferExplanation {
        def descr = "Type application \n to check validity of \n implicit conversion"
        override def toString = "Type application of arguments for selected implicit view. This will check its applicability."
      }

      case class InferExprInstanceCheckCompat(tree: Tree)
        extends Explanation with TreeInfo {
        def descr = "Infer expression instance \n (check type compatibility)"
        override def toString = "check-compatibility-for-expr-instance-inference"
      }
  }
}