package scala.reflect
package internal
package event

trait TyperEventsUniverse {
  out: SymbolTable with EventsUniverse =>

  trait TyperEvents {
    self: EventModel =>

    import Util._
      
    trait TyperEvent {
      def tag = "type"
    }

    // Type & adapt together
    trait TypingBlockEvent {
    }

    case class TyperTypeSet(tree: Tree) extends TreeEvent with TyperEvent {
      override def tag = "type-set"
    }

    case class TyperDone(originEvent: Int, status0: Boolean = true) extends Event
      with TyperEvent with DoneBlock {
      override def tag = "general-typer-done"
      protected def participants: List[Any] = List()
      override def status = status0
    }

    case class TyperTyped(tree: Tree, pt: Type)(implicit val expl: Explanation)
      extends TreeEvent with TypingBlockEvent with SymbolReferencesEvent {
      def tag = "typecheck-tree"
      def references = expl match {
        case sRefs: SymRefsInfo =>
          sRefs.refs
        case _ => List()
      }
    }
    case class TyperTypedDone(tree: Tree, originEvent: Int)
      extends TreeEvent with TypingBlockEvent with DoneBlock {
      def tag = "typecheck-done"
    }

    case class TyperTyped1(tree: Tree, pt: Type) extends TreeEvent with TyperEvent {
      override def tag = "type-tree"
    }

    case class TyperTyped1Done(originEvent: Int) extends Event with TyperEvent with DoneBlock {
      override def tag = "typed1-done"
      def participants: List[Any] = List()
    }

    case class SymInitializeTyper(sym: Symbol) extends SymEvent with TyperEvent {
      //override def tag = super.tag + "-tree-sym-init"
      override def tag = "initialize-tree-symbol"
    }
    
    case class TyperOmittedStatement(tree: Tree) extends TreeEvent with TyperEvent {
      override def tag = "targeted-compile-omit-stat"
    }



    // Package events ------------------
    trait PackageTyperEvent extends TyperEvent {
      override def tag = super.tag + "-package"
    }

    case class PackageTyper(tree: RefTree, stats: List[Tree]) extends TreeEvent with PackageTyperEvent {
    }

    // Class events --------------------
    trait ClassTyperEvent extends TyperEvent {
      override def tag = super.tag + "-class"
    }

    case class ClassTyper(tree: Tree) extends TreeEvent with ClassTyperEvent {
    }

    // Module events --------------------
    trait ModuleTyperEvent extends TyperEvent {
      override def tag = super.tag + "-object"
    }

    case class ModuleTyper(tree: Tree) extends TreeEvent with ModuleTyperEvent {
    }

    case class InitializedCompanionClassConstrs(linked: Symbol)
      extends Event with ModuleTyperEvent {
      override def tag = super.tag + "-constrs"
      def participants = List(linked)
    }

    trait ParentTypesEvent extends TyperEvent {
      override def tag = super.tag + "-parents-types"
    }

    case class ParentTypesTyper(parents: List[Tree])
      extends Event with ParentTypesEvent with TreeReferencesEvent {
      override def tag = "determine-parents'-types"
      def participants = parents
      def references = parents
    }

    case class TypeInitialSuperTpeTyper(supertpt: Tree)
      extends TreeEvent with ParentTypesEvent {
      def tree = supertpt
      override def tag = "type-first-parent-as-supertype"
    }

    case class NewSuperTpePossiblyClass(supertpt0: Tree,  supertpt: Tree)
      extends TreeEvent with ParentTypesEvent {
      def tree = supertpt
      override def tag = "replace-non-class-supertype"
    }

    case class SuperTpeParent(supertpt: Tree, supertparams: List[Symbol])
      extends TreeEvent with ParentTypesEvent {
      override def tag = "final-super-type"
      def tree = supertpt
    }

    case class SuperTpeToPolyTpeTypeConstr(newSupertpt: Type)
      extends Event with ParentTypesEvent {
      def participants = List(newSupertpt)
      override def tag = super.tag + "-poly-super-type"
    }

    case class ConvertConstrBody(params: List[List[ValDef]], body: Tree)
      extends TreeEvent with ParentTypesEvent {
      def tree = body
      override def tag = "convert-constructor"
    }

    trait ValidateParentClassEvent extends TyperEvent

    case class ValidateParentClass(parent: Tree, superclazz: Symbol)
      extends TreeEvent with ValidateParentClassEvent {
      override def tag = "validate-parent-class"
      def tree = parent
    }

    case class ValidateSelfTpeSubtypingTyper(selfTpe: Type, parentTpe: Type,
      subtypingRes: Boolean)
      extends Event with ValidateParentClassEvent with DebugEvent {
      override def tag = "validate-is-self-type-a-subtype-of-parent-type"
      def participants = List(selfTpe, parentTpe)
    }

    // ValDef events --------------------
    trait ValDefTyperEvent extends TyperEvent {
      override def tag = super.tag + "-valdef"
    }

      // TODO should take all the parameters of a valdef
    case class ValDefTyper(valdef: Tree)
      extends TreeEvent with ValDefTyperEvent {
      def tree = valdef
      override def tag = "type-value-def"
    }

    // DefDef events --------------------
    trait DefDefTyperEvent extends TyperEvent {
      override def tag = super.tag + "-defdef"
    }

    case class DefDefTyper(defdef: Tree)
      extends TreeEvent with DefDefTyperEvent {
      def tree = defdef
      override def tag = "type-definition"
    }

    // TypeDef events --------------------
    trait TypeDefTyperEvent extends TyperEvent {
      override def tag = super.tag + "-typedef"
    }

      // TODO should take all the parameters of a typedef
    case class TypeDefTyper(typedef: Tree)
      extends TreeEvent with TypeDefTyperEvent {
      def tree = typedef
    }

    // LabelDef events --------------------
    trait LabelDefTyperEvent extends TyperEvent {
      override def tag = super.tag + "-labeldef"
    }

    case class LabelDefTyper(tree: Tree)
      extends TreeEvent with LabelDefTyperEvent {
    }

    // DocDef events --------------------
    trait DocDefTyperEvent extends TyperEvent {
      override def tag = super.tag + "-docdef"
    }

    case class DocDefTyper(tree: Tree)
      extends TreeEvent with DocDefTyperEvent {
    }


    // Handle specific cases
    trait AnnotatedTyperEvent extends TyperEvent {
      override def tag = super.tag + "-annotated"
    }

    case class AnnotatedTyper(ann: Tree) extends TreeEvent with AnnotatedTyperEvent {
      def tree = ann
    }

    // Start typed Block
    trait BlockTyperEvent extends TyperEvent {
      override def tag = super.tag + "-block"
    }

    case class BlockTyper(block: Tree)
      extends TreeEvent with BlockTyperEvent {
      def tree = block
    }


    // Start typed Alternative
    trait AlternativeTyperEvent extends TyperEvent {
      override def tag = super.tag + "-alternative"
    }

    case class AlternativeTyper(alternative: Tree)
      extends TreeEvent with AlternativeTyperEvent {
      def tree = alternative
    }


    // Start typed Star
    trait StarTyperEvent extends TyperEvent {
      override def tag = super.tag + "-star"
    }

    case class StarTyper(tree: Tree)
      extends TreeEvent with StarTyperEvent {
    }

    // Start typed Bind
    trait BindTyperEvent extends TyperEvent {
      override def tag = super.tag + "-bind"
    }

    case class BindTyper(body: Tree)
      extends TreeEvent with BindTyperEvent {
      def tree = body
    }

    // Start typed UnApply
    trait UnApplyTyperEvent extends TyperEvent {
      override def tag = super.tag + "-unapply"
    }

    case class UnApplyTyper(fun: Tree, args: List[Tree])
      extends TreeEvent with UnApplyTyperEvent {
      def tree = fun
    }

    // Start typed ArrayValue
    trait ArrayValueTyperEvent extends TyperEvent {
      override def tag = super.tag + "-arrayvalue"
    }

    case class ArrayValueTyper(elemtpt: Tree, elems: List[Tree])
      extends TreeEvent with ArrayValueTyperEvent {
      def tree = elemtpt
    }

    // Start typed Function
    trait FunctionTyperEvent extends TyperEvent {
      override def tag = super.tag + "-function"
    }

    case class FunctionTyper(tree: Tree, constr: Boolean)
      extends TreeEvent with FunctionTyperEvent {
    }


    // Start typed Assign ------------------
    trait AssignTyperEvent extends TyperEvent {
      override def tag = super.tag + "-assign"
    }

    case class AssignTyper(value1: Tree, value2: Tree) extends BinaryEvent[Tree]
      with AssignTyperEvent {
      def binaryOp = "="
      override def eventString = eventStringRep
    }

    case class AssignLeftTyper(tree: Tree, tpe: Type, treeSym: Symbol)
      extends TreeTypeEventUnary with AssignTyperEvent {
      override def tag = super.tag + "-left"
    }

    case class AssignGetterTyper(tree: Tree, tpe: Type) extends TreeEvent
      with AssignTyperEvent {
      override def tag = super.tag + "-getter"
    }

    case class AssignRightTyper(tree: Tree, tpe: Type) extends TreeEvent
      with AssignTyperEvent {
      override def tag = super.tag + "-right"
    }

    // If events   ---------------------
    trait IfTyperEvent extends TyperEvent {
      override def tag = super.tag + "-if"

    }

    case class IfTyper(cond: Tree, thenp: Tree, elsep: Tree) extends Event
      with IfTyperEvent {
      protected def participants: List[Any] = List(cond, thenp, elsep)
    }

    // value: current type of tree
    case class IfCondTyper(tree: Tree)
      extends TreeEvent with IfTyperEvent {
      override def tag = super.tag + "-cond"
    }

    case class IfBranchTyper(tree: Tree, pt: Type, cond: Boolean)
      extends TreeEvent with IfTyperEvent {

      override def tag = super.tag + "-" + branch
      def branch = if (cond) "then" else "else"
    }

    case class IfLubTyper(tree1: Tree, tree2: Tree, value1: Type, value2: Type, pt: Type, expected: Tuple2[Type, Boolean])
      extends TreeTypeEventBinary with IfTyperEvent {
      def binaryOp = "ptOrLub"
      override def tag = super.tag + "-ptOrLub"
    }

    // Start typed Match
    trait MatchTyperEvent extends TyperEvent {
      override def tag = super.tag + "-match"
    }

    case class MatchTyper(tree: Tree)
      extends TreeEvent with MatchTyperEvent {
    }

    // Start typed Return
    case class ReturnTyper(tree: Tree, tpe: Type) extends TreeTypeEventUnary
      with AssignTyperEvent {
      override def tag = super.tag + "-return"
    }

    // Start typed Try
    trait TryTyperEvent extends TyperEvent {
      override def tag = super.tag + "-try"
    }

    case class TryTyper(block: Tree, catches: List[Tree], fin: Tree)
      extends TreeEvent with TryTyperEvent {
      def tree = block
    }

    // Start typed Throw
    trait ThrowTyperEvent extends TyperEvent {
      override def tag = super.tag + "-throw"
    }

    case class ThrowTyper(tree: Tree)
      extends TreeEvent with ThrowTyperEvent {
    }

    // Start typed New
    trait NewTyperEvent extends TyperEvent {
      override def tag = super.tag + "-new"
    }

    case class NewTyper(tree: Tree) extends TreeEvent
      with NewTyperEvent {
    }

    case class NewTypeCtorTyper(tree: Tree) extends TreeEvent
      with NewTyperEvent {
      override def tag = super.tag + "-typeCtor"
    }

    case class NewTypeCtorWithParamsTyper(tree: Tree, params: List[Symbol]) extends TreeEvent
      with NewTyperEvent {
      override def tag = super.tag + "-typeCtorWithParams"
    }

    // Start typed Eta
    trait EtaTyperEvent extends TyperEvent {
      override def tag = super.tag + "-eta"
    }

    case class EtaTyper(tree: Tree, tpe: Type, pt: Type)
      extends TreeTypeEventUnary with EtaTyperEvent {
    }

    case class EtaByNameParamTyper(tree: Tree, tpe: Type)
      extends TreeTypeEventUnary with EtaTyperEvent {
      override def tag = super.tag + "-by-name"
    }

    case class EtaEmptyPolyTyper(tree: Tree, tpe: Type) extends TreeTypeEventUnary
      with EtaTyperEvent {
      override def tag = super.tag + "-no-params-poly"
    }

    case class EtaPolyDoneTyper(tree: Tree, tpe: Type)
      extends TreeTypeEventUnary with EtaTyperEvent {
      override def tag = super.tag + "-poly-type-done"
    }

    case class EtaPolyAdaptTyper(tree: Tree, formals: List[Symbol])
      extends TreeEvent with EtaTyperEvent {
      override def tag = super.tag + "-poly-type-with-params"
    }

    case class EtaMethodTypeTyper(tree: Tree, params: List[Symbol])
      extends TreeEvent with EtaTyperEvent {
      override def tag = "perform-eta-expansion-adaption-for-method"
    }

    case class EtaMethodDoneTyper(tree: Tree, tpe: Type)
      extends TreeTypeEventUnary with EtaTyperEvent {
      override def tag = super.tag + "-method-done"
    }

    case class EtaMethodAdaptTyper(tree: Tree, formals: List[Symbol])
      extends TreeEvent with EtaTyperEvent {
      override def tag = super.tag + "-method-adapt"
    }

    case class TryTypedArgsTyper(args: List[Tree], mode: Int)
      extends Event with TyperEvent {
      override def tag = "typecheck-all-arguments-separately"
      def participants = List(args, mode)
    }

    // Start tryTypedApply

    trait TryTypedApplyEvent extends TyperEvent {
      override def tag = super.tag + "-try-typed-apply"
    }

    case class TryTypedApplyTyper(fun: Tree, args: List[Tree], pt: Type)
      extends TreeEvent with TreeReferencesEvent with TryTypedApplyEvent {
      def tree = fun
      override def tag = "try-typechecking-arguments-application-to-function"
      def references = args
    }

    case class SuccessTryTypedApplyTyper(tree: Tree, tpe: Type, pt: Type) extends TreeTypeEventUnary
      with TryTypedApplyEvent {
      override def tag = "successfully-typed-application"
    }

    case class SecondTryTypedApplyStartTyper(
      fun: Tree, args: List[Tree], errorMsg: String, pt: Type)
      extends TreeEvent with TryTypedApplyEvent {
      def tree = fun
      override def tag = "second-attempt:adapt-function-to-arguments"
    }

    case class SecondTryTypedApplyTyper(tree: Tree, tpe: Type, args1: List[Tree], qual1: Tree, pt: Type) extends TreeEvent
      with TryTypedApplyEvent {
      override def tag = "second-applying-arguments-to-function"
    }

    case class FailedSecondTryTypedApplyTyper(fun: Tree, args1: List[Tree], pt: Type) extends TreeEvent with TryTypedApplyEvent with HardErrorEvent {
      def tree = fun
      override def tag = "failed-adapting-qualifier-to-arguments"
    }

    case class FailedTryTypedApplyTyper(fun: Tree, args: List[Tree], pt: Type) extends Event
      with TryTypedApplyEvent with HardErrorEvent {
      def tree = fun
      override def tag = super.tag + "-failed-try"
      protected def participants: List[Any] = List(fun, args, pt)
    }

    // Start typed Typed with wildcard
    trait TypedWildcardTyperEvent extends TyperEvent {
      override def tag = super.tag + "-typedwildcard"
    }

    case class TypedWildcardTyper(tree: Tree)
      extends TreeEvent with TypedWildcardTyperEvent {
    }

    // Start typed Typed
    trait TypedTyperEvent extends TyperEvent {
      override def tag = super.tag + "-typed"
    }

    case class TypedEtaTyper(tree: Tree, expr: Tree)
      extends TreeEvent with TypedTyperEvent {
      override def tag = "typing-expr-to-be-eta-expanded"
    }

    case class TypedTypedExprTyper(tree: Tree)
      extends TreeEvent with TypedTyperEvent {
      override def tag = super.tag + "-general"
    }

    // Start typed TypeApply
    trait TypeApplyTyperEvent extends TyperEvent {
      override def tag = super.tag + "-type-apply"
    }

    // TODO: positions
    case class TypeApplyTyper(fun: Tree, args: List[Tree], pt: Type)
      extends Event with TypeApplyTyperEvent {
      protected def participants: List[Any] = List(fun, args, pt)
      override def tag = "type-type-application"
    }
    
    // Start typed Apply
    trait ApplyTyperEvent extends TyperEvent {
      override def tag = super.tag + "-apply"
    }

    case class ApplyTyper(app: Apply, e: Explanation)
      extends TreeEvent with ApplyTyperEvent with SymbolReferencesEvent {
      override def tag = "type-application"
      def tree = app
      def references = e match {
//        case TypeImplicitApplication(_, _, sym) =>
//          List(sym)
        case _ => tree.asInstanceOf[Apply].args.map(_.symbol).filter(_ != NoSymbol)
      }
    }

    // tree only for positions
    case class TypedTypeApplySuccessTyper(tree0: Tree, fun: Tree, args: List[Tree], resultTpe: Type)
      extends TreeEvent with ApplyTyperEvent {
      def tree = fun
      override def tag = super.tag + "-success"
    }

    case class TypedApplyStableTyper(tree: Tree, pt: Type)
      extends TreeEvent with ApplyTyperEvent {
      override def tag = super.tag + "-stable"
    }

    case class TypedApplyUnstableTyper(tree: Tree, patternMode: Boolean, pt: Type, args0: List[Tree])
      extends TreeEvent with ApplyTyperEvent {
      override def tag = "unstable-function"
    }

    case class SuccessTypedApplyFunTyper(tree: Tree, expectedFunPt: Type, stableApplication: Boolean, pt: Type)
      extends TreeEvent with ApplyTyperEvent with SymbolReferencesEvent {
      override def tag = "successfully-typed-function"

      def references = if (tree.symbol != null && tree.symbol.isImplicit && tree.pos == NoPosition) {
        List(tree.symbol)
      } else {
        expectedFunPt match {
          case tp: OverloadedType => tp.alternatives
          case _ => List()
        }
      }
    }

    case class TypedApplyToAssignment(fun: Tree, qual: Tree, name: Name, args: List[Tree])
      extends Event with ApplyTyperEvent {
      override def tag = super.tag + "-convert-to-assignment"
      protected def participants: List[Any] = List(fun, qual, name, args)
    }

    // Start typed ApplyBlock
    trait ApplyBlockTyperEvent extends TyperEvent {
      override def tag = super.tag + "-applyblock"
    }

    case class ApplyBlockTyper(tree: Tree)
      extends TreeEvent with ApplyBlockTyperEvent {
    }

    // Start typed ApplyDynamic
    trait ApplyDynamicTyperEvent extends TyperEvent {
      override def tag = super.tag + "-applydynamic"
    }

    case class ApplyDynamicTyper(tree: Tree)
      extends TreeEvent with ApplyDynamicTyperEvent {
    }

    // Start typed Super
    trait SuperTyperEvent extends TyperEvent {
      override def tag = super.tag + "-super"
    }

    case class SuperTyper(qual: Tree)
      extends TreeEvent with SuperTyperEvent {
      def tree = qual
    }

    // Start typed This
    trait ThisTyperEvent extends TyperEvent {
      override def tag = super.tag + "-this"
    }

    case class ThisTyper(name: Name)
      extends Event with ThisTyperEvent {
      def participants = List(name)
    }

    // Start typed Select
    trait SelectTyperEvent extends TyperEvent {
      override def tag = super.tag + "-select"
    }

    case class SelectTyper(qual: Tree, name: Name, origTree: Tree, pt: Type)
      extends TreeEvent with SelectTyperEvent {
      def tree = origTree
      override def tag = "type-full-selection"
    }

    case class SelectTreeTyper(tree: Select, pt: Type, constr: Boolean)
      extends TreeEvent with SelectTyperEvent {
      override def tag = if (constr) "type-new-object" else (super.tag + "-tree")
    }

    case class SelectConstrTyper(qual: Tree)
      extends TreeEvent with SelectTyperEvent {
      def tree = qual
      override def tag = "type-super-constructor-call"
    }

    case class TreeSymSelectTyper(sym: Symbol)
      extends SymEvent with SelectTyperEvent with DebugEvent {
      override def tag = super.tag + "-qualSym"
    }

    case class SymSelectTyper(qual: Tree, member: Name, sym: Symbol)
      extends TreeEvent with SelectTyperEvent {
      override def tag = (if (sym == NoSymbol) "qualifier-without-symbol" else "qualifier-with-symbol")
      def tree = qual
    }

    case class SymExistsSelectTyper(sym: Symbol)
      extends SymEvent with SelectTyperEvent {
      override def tag = super.tag + "-sym-exists"
    }

    case class StabilizeTreeTyper(tree: Tree, sym: Symbol, tpe: Type)
      extends TreeEvent with SelectTyperEvent {
      override def tag = super.tag + "-tree-after-stabilize"
    }

    case class DeferredTypeTreeTyper(tree: Tree, tpeArgs: List[Type])
      extends TreeEvent with SelectTyperEvent {
      override def tag = super.tag + "-deferredTypeTree"
      override def eventString = super.eventString + " typeArgs: " + tpeArgs
    }

    // Start typed Ident
    trait IdentTyperEvent extends TyperEvent {
      override def tag = super.tag + "-ident"
    }

    case class IdentTyper(tree: Ident)
      extends TreeEvent with IdentTyperEvent {
    }

    // Start typed Literal
    trait LiteralTyperEvent extends TyperEvent {
      override def tag = super.tag + "-literal"
    }

    case class LiteralTyper(tree: Literal, pt: Type)
      extends TreeEvent with LiteralTyperEvent {
    }
    
    case class ReferenceToBoxedTyper(tree: Ident)
      extends TreeEvent with TyperEvent {
      override def tag = super.tag + "-reference-to-boxed"
    }


    // Start typed SingletonTypeTree
    trait SingletonTypeTreeTyperEvent extends TyperEvent {
      override def tag = super.tag + "-singleton"
    }

    case class SingletonTypeTreeTyper(tree: Tree)
      extends TreeEvent with SingletonTypeTreeTyperEvent {
    }


    // Start typed SelectFromTypeTree
    trait SelectFromTypeTreeTyperEvent extends TyperEvent {
      override def tag = super.tag + "-selecttypetree"
    }

    case class SelectFromTypeTreeTyper(qual: Tree, select: Name)
      extends TreeEvent with SelectFromTypeTreeTyperEvent {
      def tree = qual
    }


    // Start typed CompoundTypeTree
    trait CompoundTypeTyperEvent extends TyperEvent {
      override def tag = super.tag + "-compoundtype"
    }

    case class CompoundTypeTyper(tree: Tree)
      extends TreeEvent with CompoundTypeTyperEvent {
    }


    // Start typed AppliedTypeTree
    trait AppliedTypeTyperEvent extends TyperEvent {
      override def tag = super.tag + "-appliedtype"
    }

    case class AppliedTypeTyper(tree: Tree, args: List[Tree])
      extends TreeEvent with AppliedTypeTyperEvent {
      override def tag = "type-applied-type"
    }


    // Start typed TypeBoundsTree
    trait TypeBoundsTyperEvent extends TyperEvent {
      override def tag = super.tag + "-type-bounds"
    }

    // TODO: there seems to be a bug related to range positions, hence
    // def foo[T] will not show correctly the position for T
    case class TypeBoundsTyper(bounds: Tree, treel: Tree, treeh: Tree)
      extends TreeEvent with TypeBoundsTyperEvent {
      def tree = bounds
      override def participants = List(treel, treeh)
    }


    // Start typed ExistentialType
    trait ExistentialTypeTyperEvent extends TyperEvent {
      override def tag = super.tag + "-existential"
    }

    case class ExistentialTypeTyper(tree: Tree)
      extends TreeEvent with ExistentialTypeTyperEvent {
    }


    // Start typed DeferredRefCheckType
    trait DeferredRefCheckTypeTyperEvent extends TyperEvent {
      override def tag = super.tag + "-deferredrefcheck"
    }

    case class DeferredRefCheckTypeTyper(tree: Tree)
      extends TreeEvent with DeferredRefCheckTypeTyperEvent {
    }


    // Start typed TypeTree
    trait TypeTreeTyperEvent extends TyperEvent {
      override def tag = super.tag + "-typetree"
    }

    case class TypeTreeTyper(tree: Tree)
      extends TreeEvent with TypeTreeTyperEvent {
    }


    // Start typed Import
    trait ImportTyperEvent extends TyperEvent {
      override def tag = super.tag + "-import"
    }

    case class ImportTyper(tree: Tree, selectors: List[ImportSelector])
      extends TreeEvent with ImportTyperEvent {
    }


    // Start typed Unexpected
    trait UnexpectedTyperEvent extends TyperEvent {
      override def tag = super.tag + "-unexpected"
    }

    case class UnexpectedTyper(tree: Tree)
      extends TreeEvent with UnexpectedTyperEvent {
    }


    trait TemplateTyperEvent extends TyperEvent {
      override def tag = super.tag + "-template"
    }

    case class TemplateTyper(templ: Template, parents: List[Tree],
        clazz: Symbol)(implicit val info: TemplateInfo)
      extends TreeEvent with TemplateTyperEvent {
      def tree = templ
      override def lName =
        info match {
          case ClassTemplate  => "Type class template"
          case ModuleTemplate => "Type object template"
          case TraitTemplate => "Type trait template"
        }
      override def participants = List(tree, parents)
    }

    case class SelfTpeRefinedType(parents: List[Type], owner: Symbol)
      extends Event with TemplateTyperEvent {
      override def tag = super.tag + "-refined-self-type"
      override def participants = parents
    }

    case class SelfTpeThis(clazz: Symbol)
      extends Event with TemplateTyperEvent {
      override def tag = super.tag + "-type-of-this"
      def participants = List(clazz)
    }

    // ----------------------- adapt to arguments/member
    // -----------------------
    // -----------------------
    trait AdaptToEvent

    trait AdaptToArgumentsEvent extends AdaptToEvent {
      def tag = "adaptToArguments"
    }

    case class AdaptToArgumentsTyper(tree: Tree, name: Name, args: List[Tree], pt: Type)
      extends TreeEvent with AdaptToArgumentsEvent {
      override def tag = "adapt-function-to-arguments"
    }

    case class FinishedAdaptToArgumentsTyper(value1: Tree, value2: Tree,
        isWildcard: Boolean, originEvent: Int)
      extends TwoTreeEvent with AdaptToArgumentsEvent with DoneBlock {
      override def tag =
        (if (value1 eq value2) "failed-to-adapt-function-to-arguments"
        else "successfully-adapted-function-to-arguments")
      def binaryOp = ""
    }

    case class FallbackAdaptToArgumentsTyper(tree: Tree, error: String)
      extends TreeEvent with AdaptToArgumentsEvent {
      override def tag = "fallback:try-to-adapt-with-no-expected-type"
    }

    trait AdaptToMemberEvent extends AdaptToEvent {
      def tag = "adaptToMember"
    }

    case class AdaptToMemberTyper(tree: Tree, tpe: Type, searchTpe: Type)
      extends TreeEvent with AdaptToMemberEvent {
      override def tag = "adapt-to-member"
    }

    case class OpenExistentialAdaptToMemberTyper(tpe: Type)
      extends TypeEvent with AdaptToMemberEvent {
      override def tag = super.tag + "-openExistential"
    }

    // todo obsolete?
    case class FoundCoercionAdapToMemberTyper(qual: Tree, coercion: Tree)
      extends TreeEvent with TreeReferencesEvent with AdaptToMemberEvent {
      def tree = qual
      override def tag = super.tag + "-found-coercion"
      def references = List(coercion)
    }

    case class FailedAdaptToMemberTyper(tree: Tree, tpe: Type, searchTpe: Type, originEvent: Int)
      extends TreeEvent with AdaptToMemberEvent
      with SoftErrorEvent with DoneBlock {
      override def tag = super.tag + "-failed"
    }

    case class IsNotAdaptableTyper(tree: Tree, tpe: Type)
      extends TreeEvent with AdaptToMemberEvent with SoftErrorEvent {
      override def tag = super.tag + "-unaddaptable"
    }

    case class InferViewAdaptToMemberTyper(value1: Type, value2: Type)
      extends Event with AdaptToMemberEvent {
      override def tag = super.tag + "-infer"
      def participants = List(value1, value2)
    }

    // ----------------------------------
    // --------- Do typed apply ---------
    // ----------------------------------
    trait DoTypedApplyEvent {
      def tag = "doTypedApply"
    }

    case class DoTypedApplyTyper(tree0: Tree, fun: Tree, args: List[Tree], mode: Int, pt: Type)
    extends TreeEvent with TreeReferencesEvent with DoTypedApplyEvent {
      def tree = fun
      def references = args
      override def tag = "typecheck-arguments-application-to-function"
    }

    // overloaded sym
    case class OverloadedSymDoTypedApply(tree: Tree, sym: Symbol,
        argTypes: List[Type], pre: Type)
      extends TreeEvent with DoTypedApplyEvent with SymbolReferencesEvent {
      override def tag = "quick-alternatives-filter-for-overloaded-function"
      def references = sym.info.asInstanceOf[OverloadedType].alternatives
    }

    case class CheckApplicabilityAlternativeDoTypedApply(
      tree: Tree, funSym: Symbol, pt: Type, alt: Symbol)
      extends TreeEvent with DoTypedApplyEvent with SymbolReferencesEvent {
      override def tag = super.tag + "-check-alternative-symbol"
      def sym = funSym
      def references = List(alt)
    }

    case class IsApplicableAlternativeDoTypedApply(sym: Symbol,
        ftpe: Type, originEvent: Int, applicable: Boolean)
      extends SymEvent with DoTypedApplyEvent with DoneBlock {
      override def tag = super.tag + "-overloadedSym-applicable"
    }

    case class FilteredDoTypedApply(tree: Tree, funSym: Symbol, originEvent: Int)
      extends TreeEvent with DoTypedApplyEvent with DoneBlock {
      override def tag = "filtered-out-alternatives"
    }

    case class OverloadedTpeDoTypedApply(fun: Tree, pre: Type, alts: List[Symbol])
      extends Event with DoTypedApplyEvent with SymbolReferencesEvent {
      override def tag = "typecheck-application-for-overloaded-method"
      def tree = fun
      def participants = alts
      def references = alts
    }

    case class InferMethodAlternativeDoTypedApply(fun: Tree)
      extends TreeEvent with DoTypedApplyEvent {
      def tree = fun
      override def tag = "infer-correct-method-for-application-from-alternatives-"
    }

    case class AdaptInferredMethodAlternativeDoTypedApply(fun: Tree)
      extends TreeEvent with DoTypedApplyEvent with SymbolReferencesEvent {
      def tree = fun
      override def tag = "adapt-inferred-method-alternative"
      def references = List(tree.symbol)
    }

    case class InferredMethodDoTypedApply(fun: Tree)
      extends TreeEvent with DoTypedApplyEvent {
      def tree = fun
      override def tag = "typecheck-arguments-application-for-the-inferred-method"
    }

    case class MethodTpeDoTypedApply(params: List[Symbol], paramTypes: List[Type])
      extends Event with DoTypedApplyEvent {
      override def tag = "function-with-method-type"
      def participants = List(params)
    }

    case class TryTupleApplyDoTypedApply(params: List[Symbol])
      extends Event with DoTypedApplyEvent {
      override def tag = super.tag + "-try-tuple-apply"
      def participants = List(params)
    }

    case class PackArgsDoTypedApply(tupleArgs: List[Tree])
      extends Event with DoTypedApplyEvent {
      override def tag = super.tag + "-tuple-packed"
      def participants = List(tupleArgs)
    }

    case class PackedArgsDoTypedApply(tree: Any, originEvent: Int, status0: Boolean)
      extends Event with DoTypedApplyEvent with DoneBlock {
      override def tag = super.tag + "-packedArgs[" + (if (status) "OK" else "FAILED") + "]"
      override def status = status0
      def participants = List(tree)
    }

    case class TryNamesDefaultsDoTypedApply(args: List[Tree], formals: List[Type])
      extends Event with DoTypedApplyEvent {
      override def tag = super.tag + "-tryNamesDefaults"
      def participants = List(args, formals)
    }

    case class CorrectArgumentsDoTypedApply(tree: Tree, formals: List[Type], args: List[Tree])
      extends TreeEvent with DoTypedApplyEvent {
      override def tag = "correct-number-of-arguments"
    }

    case class TParamsResolvedDoTypedApply(tree: Tree)
      extends TreeEvent with DoTypedApplyEvent {
      override def tag = "all-type-parameters-are-resolved"
    }

    case class TypedArgsDoTypedApply(args: List[Tree], originEvent: Int)
      extends Event with DoTypedApplyEvent with DoneBlock {
      def participants = List(args)
      override def tag = "arguments-successfully-typed"
    }

    case class ApplyTreeDoneDoTypedApply(tree: Tree, originEvent: Int)
      extends TreeEvent with DoTypedApplyEvent with DoneBlock {
      override def tag = "successfully-typed-application"
    }

    case class NeedsInstantiationDoTypedApply(tparams: List[Symbol], formals: List[Type], args0: List[Tree])
      extends Event with DoTypedApplyEvent {
      override def tag = super.tag + "-needsInstantiation"
      def participants = List(tparams)
    }

    case class MethodTpeWithUndetTpeParamsDoTypedApply(fun: Tree, tparams: List[Symbol],
      formals: List[Type]) extends TreeEvent with DoTypedApplyEvent {
      def tree = fun
      override def tag = "method-with-undetermined-type-parameters"
    }

     case class ProtoTypeArgsDoTypedApply(tparams: List[Symbol], formals: List[Type],
      resultTpe: Type, pt: Type)
      extends Event with DoTypedApplyEvent {
      override def tag = super.tag + "-prototypeargs"
      def participants = tparams
    }

    case class InstantiatedDoTypedApply(fun: Tree, pt: Type, undet: List[Symbol])
      extends TreeEvent with DoTypedApplyEvent {
      def tree = fun
      override def tag = "typecheck-inferred-instance-in-the-application"
    }

    case class DoTypedApplyDone(originEvent: Int, tree: Tree)
      extends TreeEvent with DoTypedApplyEvent with DoneBlock {
      override def tag = super.tag + "-done"
    }

    case class SingleTpeDoTypedApply(tpe: Type)
      extends TypeEvent with DoTypedApplyEvent {
      override def tag = super.tag + "-singleTpe"
    }

    case class ErrorTpeDoTypedApply(tree: Tree)
      extends TreeEvent with DoTypedApplyEvent {
      override def tag = super.tag + "-errorTpe"
    }

    case class UnapplyDoTypedApply(sym: Symbol)
      extends SymEvent with DoTypedApplyEvent {
      override def tag = super.tag + "-unapply"
    }
  }

  trait TyperExplanations {
    self: EventModel =>

    trait TyperExplanation
    
    // todo: put information into Explanation
    trait StatementExplanation {
      def stat: Tree
    }

    // Information
    trait TemplateInfo

    case object ClassTemplate extends TemplateInfo
    case object ModuleTemplate extends TemplateInfo
    case object TraitTemplate extends TemplateInfo
    
    //Unit
    case class TypeUnit(unit: CompilationUnit)
      extends Explanation with TyperExplanation

    //Package
    case class TypePackageQualifier(qual: RefTree)
      extends Explanation with TyperExplanation

    case class TypePackageStatement(stat: Tree)
      extends Explanation with TyperExplanation with StatementExplanation

    // Template
    case class TypeTemplateStatement(stat: Tree)(implicit info: TemplateInfo)
      extends Explanation with TyperExplanation with StatementExplanation {
      def templateInfo = info
    }


    // DefDef
    case class TypeExplicitTreeReturnType(tree: Tree, tpe: Tree)
      extends Explanation with TyperExplanation

    case class TypeDefConstr(ddef: DefDef)
      extends Explanation with TyperExplanation

    case class TypeDefBody(ddef: DefDef)
      extends Explanation with TyperExplanation

    // Apply
    case class TypeFunctionApply(fun: Tree)
      extends Explanation with TyperExplanation

    case class TypeArgsApply(args: List[Tree])
      extends Explanation with TyperExplanation {

      override def provide(a: Tree): Explanation = TypeArgApply(a)
    }

    case class TypeArgApply(arg: Tree)
      extends Explanation with TyperExplanation

    // Apply overloaded
    case class TypeArgsInOverloadedApply(args: List[Tree])
      extends Explanation with TyperExplanation {

      override def provide(a: Tree): Explanation = TypeArgInOverloadedApply(a)
    }

    case class TypeArgInOverloadedApply(arg: Tree)
      extends Explanation with TyperExplanation

    //Eta
    case class TypeEtaExpandedTreeWithWildcard(tree: Tree)
      extends Explanation with TyperExplanation

    case class TypeEtaExpandedTreeWithPt(tree: Tree)
      extends Explanation with TyperExplanation

    //TypeApply
    case class TypeTypeConstructorTypeApply(fun: Tree)
      extends Explanation with TyperExplanation

    case class TypeHigherKindedTypeApplyWithExpectedKind(arg: Tree, pt: Type)
      extends Explanation with TyperExplanation

    case class TypeHigherKindedTypeApplyOverloaded(arg: Tree)
      extends Explanation with TyperExplanation

    case class TypeHigherKindedTypeForAppliedType(arg: Tree)
      extends Explanation with TyperExplanation


    //TypeFunction
    case class TypeFunctionParameter(par: ValDef)
      extends Explanation with TyperExplanation

    case class TypeFunBody(fun: Tree)
      extends Explanation with TyperExplanation

    //Do typed apply, try typed apply
    case class TypeArgStandalone(arg: Tree)
      extends Explanation with TyperExplanation

    //Block
    case class TypeStatementInBlock(stat: Tree)(implicit underlying0: Explanation)
      extends Explanation with TyperExplanation with StatementExplanation {
      
      override def underlying = underlying0
    }

    case class TypeLastStatementInBlock(stat: Tree)(implicit underlying0: Explanation)
      extends Explanation with TyperExplanation with StatementExplanation {

      override def underlying = underlying0
    }

    //Select
    case class TypeQualifierInSelect(qual: Tree)
      extends Explanation with TyperExplanation

    case class TypeSuperQualifier(qual: Tree)
      extends Explanation with TyperExplanation

    case class TypeAdaptedQualifer(qual: Tree, name: Name)
      extends Explanation with TyperExplanation

    //Super
    case class TypeQualifierInSuper(qual: Tree)
      extends Explanation with TyperExplanation

    //AdaptToMember
    case class TypeAppliedImplicitView(view: Tree)
      extends Explanation with TyperExplanation

    //ValDef
    case class TypeValType(vdef: Tree)
      extends Explanation with TyperExplanation

    //Type constructor
    case class TypeTypeConstructorInNew(tpt: Tree)
      extends Explanation with TyperExplanation

    //Parent typing

    case class TypeInitialSuperType(supertpt: Tree)
      extends Explanation with TyperExplanation {
    }

    case class TypeParentMixin(mixin: Tree)
      extends Explanation with TyperExplanation {
    }

    case class TypeCurrentTraitSuperTpt(supertpt: Tree)
      extends Explanation with TyperExplanation

    case class TypeFirstConstructor(body: Tree)
      extends Explanation with TyperExplanation

    //Typed
    case class TypeExplicitTypeAnnotation(tpt: Tree)
      extends Explanation with TyperExplanation

    case class TypeAnnotatedExpr(expr: Tree)
      extends Explanation with TyperExplanation

    // Bounds checking
    case class TypeLowerBound(bound: Tree)
      extends Explanation with TyperExplanation

    case class TypeHigherBound(bound: Tree)
      extends Explanation with TyperExplanation

    //type parameters
    case class TypeClassTypeParameter(tparam: Tree)
      extends Explanation with TyperExplanation

    case class TypeHigherOrderTypeParameter(tparam: Tree)
      extends Explanation with TyperExplanation

    case class TypeDefTypeParameter(tparam: Tree)
      extends Explanation with TyperExplanation

    case class TypeDefParameter(param: Tree)
      extends Explanation with TyperExplanation

    // refinement
    case class TypeRefinementStatement(stat: Tree)
      extends Explanation with TyperExplanation with StatementExplanation

    case class TypeExistentialTypeStatement(stat: Tree)
      extends Explanation with TyperExplanation with StatementExplanation

    // Arg
    case class TypeArgForCorrectArgsNum(arg: Tree)
      extends Explanation with TyperExplanation

    case class TypeArgWithLenientPt(arg: Tree, pt: Type)
      extends Explanation with TyperExplanation

    // Use case?
    case class TypeUseCaseStatement(stat: Tree)
      extends Explanation with TyperExplanation with StatementExplanation


    // Other [adapt]
    case class AdaptQualToMemberInferView(qual: Tree, to: Type)
      extends Explanation with TyperExplanation
  }
}