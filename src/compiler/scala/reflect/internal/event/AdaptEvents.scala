package scala.reflect
package internal
package event

trait AdaptEventsUniverse {
  out: SymbolTable with EventsUniverse =>

  trait AdaptEvents {
    self: EventModel =>

    trait AdaptEvent {
      def tag = "adapt"
    }


    case class AdaptStart(tree: Tree, pt: Type)
      extends TreeEvent with AdaptEvent {
      var resTree: (Tree, Clock) = (tree, time) // update-able, make it a def
    }

    case class AdaptDone(originEvent: Int, status0: Boolean = true)
      extends Event with AdaptEvent with DoneBlock {
      override def tag = super.tag + "-done"
      override def participants: List[Any] = List()
      override def status = status0
    }
    
    case class MainAdaptDone(originEvent: Int, resTree: Tree) extends Event with AdaptEvent with DoneBlock {
      override def tag = super.tag + "-tree-adapt-done"
      def participants: List[Any] = List()
    }

    case class AdaptAnnotationsAdapt(tree: Tree)
      extends TreeEvent with AdaptEvent {
      override def tag = super.tag + "-annotations"
    }

    trait ConstantTpeAdaptEvent extends AdaptEvent {
      override def tag = super.tag + "-constanttpe"
    }

    case class ConstantTpeAdapt(tree: Tree, tpe: Type)
      extends TreeTypeEventUnary with ConstantTpeAdaptEvent {
    }

    case class OverloadedTpeAdapt(tpe: Type, alts: List[Symbol])
      extends TypeEvent with AdaptEvent {
      override def tag = super.tag + "-overloaded"
    }

    case class NullaryMethodTypeAdapt(tpe: Type, undetTParams: List[Symbol])
      extends TypeEvent with AdaptEvent {
      override def tag = super.tag + "-nullaryMethTpe"
    }

    case class ByNameParamClassAdapt(tpe: Type)
      extends TypeEvent with AdaptEvent {
      override def tag = super.tag + "-byname"
    }

    case class SkolemizeTpe1Adapt(tpe: Type)
      extends TypeEvent with AdaptEvent {
      override def tag = super.tag + "-skolemizeTypeRef"
    }

    case class SkolemizeTpe2Adapt(tpe: Type)
      extends TypeEvent with AdaptEvent {
      override def tag = super.tag + "-skolemizeExistential"
    }

    case class PolyTpeAdapt(tree: Tree, tparams: List[Symbol],
      restpe: Type, typeTree: Tree, undetTParams: List[Symbol])
      extends TreeEvent with AdaptEvent {
      override def tag = super.tag + "-poly-type"
    }

    // Start implicit method type adapt
    trait ImplicitMethodTpeAdaptEvent extends AdaptEvent

    case class ImplicitMethodTpeAdapt(tree: Tree, tpe: Type)
      extends TreeEvent with ImplicitMethodTpeAdaptEvent {
      override def tag = super.tag + "-implicit-method-type"
    }

    case class UndetParamsMethodTpeAdapt(params: List[Symbol])
      extends Event with ImplicitMethodTpeAdaptEvent {
      override def tag = super.tag + "-undetermined-type-variables"
      def participants: List[Any] = params
    }

    case class SuccessSilentMethodTpeAdapt(tree: Tree, tpe: Type)
      extends TreeEvent with ImplicitMethodTpeAdaptEvent {
      override def tag = super.tag + "-success-applied-implicit-args"
    }

    case class InferImplicitForParamAdapt(param: Symbol, pos0: String)
      extends Event with SymbolReferencesEvent with ImplicitMethodTpeAdaptEvent {
      def participants = List(param)
      override def tag = "infer-implicit-for-parameter"
      def references = List(param)
    }

    case class InferDivergentImplicitValueNotFound(param: Symbol)
      extends Event with ImplicitMethodTpeAdaptEvent with HardErrorEvent {
      override def tag = "implicit-value-not-found"
      def participants = List(param)
      def errPos = param.pos
    }


    case class InferImplicitValueNotFound(param: Symbol)
      extends Event with ImplicitMethodTpeAdaptEvent with HardErrorEvent {
      override def tag = "implicit-value-not-found"
      def participants = List(param)
      def errPos = param.pos
    }

    case class InferredImplicitAdapt(originEvent: Int)
      extends Event with ImplicitMethodTpeAdaptEvent with DoneBlock {
      override def tag = super.tag + "-parameter-done"
      def participants = List()
    }

    trait MethodTpeAdaptEvent extends AdaptEvent {
      override def tag = super.tag + "-method-type"
    }

    case class EtaMethodTpeAdapt(meth: Symbol, tree: Tree, pt: Type)
      extends TreeEvent with MethodTpeAdaptEvent {
      override def tag = "eta-expansion"
    }

    case class TreeAfterEtaExpansionMethodTpeAdapt(tree: Tree, tree1: Tree)
      extends TreeEvent with MethodTpeAdaptEvent {
      override def tag = "eta-expanded-tree"
    }

    case class InstantiateTParamsForEtaExpansionAdapt(tree: Tree, meth: Symbol,
      tparams: List[Symbol])
      extends TreeEvent with MethodTpeAdaptEvent {
      override def tag = "instantiate-type-params-in-eta-expansion"
    }

    case class InferExprFailed(tree: Tree, pt: Type, e: String)
      extends TreeEvent with MethodTpeAdaptEvent with SoftErrorEvent {
      override def tag = "failed-to-infer-expr-instance"
      def errPos = tree.pos
    }

    case class ApplyNullaryMethodAdapt(tree: Tree, methTpe: Type)
      extends TreeEvent with MethodTpeAdaptEvent {
      override def tag = "apply-nullary-method"
    }


    case class TypeTreeAdapt(tree: Tree)
      extends TreeEvent with AdaptEvent {
      override def tag = super.tag + "-typeTree"
    }

    case class FunModeAdapt(tree: Tree)
      extends TreeEvent with AdaptEvent with DebugEvent {
      override def tag = super.tag + "-funmode"
    }

    case class ConvertToTypeTreeAdapt(tree: Tree)
      extends TreeEvent with AdaptEvent {
      override def tag = super.tag + "-convertToTypeTree"
    }

    case class PatternConstructorsAdapt(tree: Tree, sym: Symbol)
      extends TreeEvent with AdaptEvent {
      override def tag = super.tag + "-patternConstructor"
    }

    case class ApplyAdapt(tree: Tree)
      extends TreeEvent with AdaptEvent {
      override def tag = "adapt-to-tree-with-apply()"
    }

    case class AdaptToNameQualAdapt(tree: Tree)
      extends TreeEvent with AdaptEvent {
      override def tag = super.tag + "-apply-qual"
    }

    case class InferInstanceAdapt(tree: Tree, tparams: List[Symbol],
      e: Explanation, pt: Type)
      extends TreeEvent with AdaptEvent {
      override def tag = super.tag + "-infer-instance"
    }

    // Try subtyping
    case class SuccessSubTypeAdapt(tree: Tree, value1: Type, value2: Type) extends TwoTypeEvent with AdaptEvent {
      override def tag = super.tag + "-subtype-ok"
      def binaryOp = "<:<"
    }

    case class ConstantFoldSubTypeAdapt(tree: Tree, value1: Type, value2: Type) extends TwoTypeEvent with AdaptEvent {
      override def tag = super.tag + "-subtype-const"
      def binaryOp = "<:<"
    }

    // Not a subtype case
    case class NotASubtypeAdapt(tpe: Type, pt: Type)
      extends TypeEvent with AdaptEvent {
      override def tag = super.tag + "-not-a-subtype"
    }

    case class AdaptToUnitAdapt(tpe: Type, pt: Type)
      extends TypeEvent with AdaptEvent {
      override def tag = super.tag + "-unit<:<Any"
    }

    case class WeakConformanceAdapt(tpe: Type, pt: Type)
      extends TypeEvent with AdaptEvent {
      override def tag = super.tag + "-weakConformance"
    }

    case class AnnotationCheckerAdapt(tree: Tree)
      extends TreeEvent with AdaptEvent {
      override def tag = super.tag + "-annotationChecker"
    }

    case class InstantiateAdapt(tree: Tree)
      extends TreeEvent with AdaptEvent {
      override def tag = super.tag + "-instantiate"
    }

    case class FindViewAdapt(tree: Tree)
      extends TreeEvent with AdaptEvent {
      override def tag = "try-to-find-view"
    }

    case class ApplyViewAdapt(coercion: Tree, tree: Tree)
      extends TreeEvent with SymbolReferencesEvent with AdaptEvent {
      override def tag = "apply-found-view"
      def references = List(coercion.symbol)
    }

    case class NoViewFound(tree: Tree, pt: Type)
      extends TreeEvent with AdaptEvent {
      override def tag = "no-view-found"
    }
  }

  trait AdaptExplanations {
    self: EventModel =>

    trait AdaptExplanation

    case class TypeQualifierWithApply(orig: Tree, qual: Tree)
      extends Explanation with AdaptExplanation

    case class MethodEtaExpansion(expr: Tree)
      extends Explanation with AdaptExplanation {
      override def descrName = "perform-eta-expansion-adaption"
    }

    case class NotASubtypeInferView(treetp: Type, pt: Type)
      extends Explanation with AdaptExplanation {
      override def toString = "infer-view-for-subtype-adaptation"
    }

    case class FirstTryTypeTreeWithAppliedImplicitArgs(tree: Tree, pt: Type)
      extends Explanation with AdaptExplanation

    case class FallbackImplicitArgsTypeClean(tree: Tree, original: Tree)
      extends Explanation with AdaptExplanation

    case class TypeTreeWithAppliedImplicitArgs(tree: Tree)
      extends Explanation with AdaptExplanation

    case class TypeImplicitViewApplication(tree: Tree, coercion: Tree)
      extends Explanation with TreeInfo with SymRefsInfo with AdaptExplanation {
      def refs = List(coercion.symbol)
    }
    
    case class TypeApplyAdapt(tree: Tree)
      extends Explanation with AdaptExplanation
  }
}