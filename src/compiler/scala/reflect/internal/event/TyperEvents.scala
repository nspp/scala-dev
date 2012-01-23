package scala.reflect
package internal
package event

trait TyperEventsUniverse {
  out: SymbolTable with EventsUniverse =>

  trait TyperEvents {
    self: EventModel =>

    trait TyperEvent {
      def tag = "type"
    }

    // Type & adapt together
    trait TypingBlockEvent {
    }

    case class TyperTypeSet(tree0: Tree) extends TreeEvent with TyperEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = "type-set"
    }

    case class TyperDone(originEvent: Int, status0: Boolean = true) extends Event
      with TyperEvent with DoneBlock {
      override def tag = "general-typer-done"
      protected def participants: List[Any] = List()
      override def status = status0
    }

    case class TyperTyped(tree0: Tree, pt: Type)(implicit val expl: Explanation)
      extends TreeEvent with TypingBlockEvent with SymbolReferencesEvent {
      def tag = "typecheck"
      def references = expl match {
        case sRefs: SymRefsInfo =>
          sRefs.refs
        case _ => List()
      }
      val tree = duplicateTreeWithPos(tree0)
    }
    case class TyperTypedDone(tree0: Tree, originEvent: Int)
      extends TreeEvent with TypingBlockEvent with DoneBlock {
      def tag = "typecheck-done"
      val tree = duplicateTreeWithPos(tree0)
    }

    case class TyperTyped1(tree0: Tree, pt: Type) extends TreeEvent with TyperEvent {
      override def tag = "type-tree"
      val tree = duplicateTreeWithPos(tree0)
    }

    case class TyperTyped1Done(originEvent: Int) extends Event with TyperEvent with DoneBlock {
      override def tag = "typed1-done"
      def participants: List[Any] = List()
    }

    case class SymInitializeTyper(sym: Symbol) extends SymEvent with TyperEvent {
      //override def tag = super.tag + "-tree-sym-init"
      override def tag = "initialize-tree-symbol"
    }



    // Package events ------------------
    trait PackageTyperEvent extends TyperEvent {
      override def tag = super.tag + "-package"
    }

    case class PackageTyper(tree0: RefTree, stats: List[Tree]) extends TreeEvent with PackageTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }

    // Class events --------------------
    trait ClassTyperEvent extends TyperEvent {
      override def tag = super.tag + "-class"
    }

    case class ClassTyper(tree0: Tree) extends TreeEvent with ClassTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }

    // Module events --------------------
    trait ModuleTyperEvent extends TyperEvent {
      override def tag = super.tag + "-object"
    }

    case class ModuleTyper(tree0: Tree) extends TreeEvent with ModuleTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }

    case class InitializedCompanionClassConstrs(linked: Symbol)
      extends Event with ModuleTyperEvent {
      override def tag = super.tag + "-constrs"
      def participants = List(linked)
    }

    trait ParentTypesEvent extends TyperEvent {
      override def tag = super.tag + "-parents-types"
    }

    case class ParentTypesTyper(parents0: List[Tree])
      extends Event with ParentTypesEvent with TreeReferencesEvent {
      override def tag = "determine-parents'-types"
      val parents = parents0.map(duplicateTreeWithPos)
      def participants = parents
      def references = parents
    }

    case class TypeInitialSuperTpeTyper(supertpt: Tree)
      extends TreeEvent with ParentTypesEvent {
      val tree = duplicateTreeWithPos(supertpt)
      override def tag = "type-first-parent-as-supertype"
    }

    case class NewSuperTpePossiblyClass(supertpt0: Tree,  supertpt: Tree)
      extends TreeEvent with ParentTypesEvent {
      val tree = duplicateTreeWithPos(supertpt)
      override def tag = "replace-non-class-supertype"
    }

    case class SuperTpeParent(supertpt: Tree, supertparams: List[Symbol])
      extends TreeEvent with ParentTypesEvent {
      override def tag = "final-super-type"
      val tree = duplicateTreeWithPos(supertpt)
    }

    case class SuperTpeToPolyTpeTypeConstr(newSupertpt: Type)
      extends Event with ParentTypesEvent {
      def participants = List(newSupertpt)
      override def tag = super.tag + "-poly-super-type"
    }

    case class ConvertConstrBody(params0: List[List[ValDef]], body: Tree)
      extends TreeEvent with ParentTypesEvent {

      val tree = duplicateTreeWithPos(body)
      val params = params0.map(_.map(duplicateTreeWithPos))
      override def tag = "convert-constructor"
    }

    trait ValidateParentClassEvent

    case class ValidateParentClass(parent: Tree, superclazz: Symbol)
      extends TreeEvent with ValidateParentClassEvent {
      override def tag = "validate-parent-class"
      val tree = duplicateTreeWithPos(parent)
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
      val tree = duplicateTreeWithPos(valdef)
      override def tag = "type-value-def"
    }

    // DefDef events --------------------
    trait DefDefTyperEvent extends TyperEvent {
      override def tag = super.tag + "-defdef"
    }

    case class DefDefTyper(defdef: Tree)
      extends TreeEvent with DefDefTyperEvent {
      val tree = duplicateTreeWithPos(defdef)
      override def tag = "type-definition"
    }

    // TypeDef events --------------------
    trait TypeDefTyperEvent extends TyperEvent {
      override def tag = super.tag + "-typedef"
    }

      // TODO should take all the parameters of a typedef
    case class TypeDefTyper(typedef: Tree)
      extends TreeEvent with TypeDefTyperEvent {
      val tree = duplicateTreeWithPos(typedef)
    }

    // LabelDef events --------------------
    trait LabelDefTyperEvent extends TyperEvent {
      override def tag = super.tag + "-labeldef"
    }

    case class LabelDefTyper(tree0: Tree)
      extends TreeEvent with LabelDefTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }

    // DocDef events --------------------
    trait DocDefTyperEvent extends TyperEvent {
      override def tag = super.tag + "-docdef"
    }

    case class DocDefTyper(tree0: Tree)
      extends TreeEvent with DocDefTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }


    // Handle specific cases
    trait AnnotatedTyperEvent extends TyperEvent {
      override def tag = super.tag + "-annotated"
    }

    case class AnnotatedTyper(ann: Tree) extends TreeEvent with AnnotatedTyperEvent {
      val tree = duplicateTreeWithPos(ann)
    }

    // Start typed Block
    trait BlockTyperEvent extends TyperEvent {
      override def tag = super.tag + "-block"
    }

    case class BlockTyper(block: Tree)
      extends TreeEvent with BlockTyperEvent {
      val tree = duplicateTreeWithPos(block)
    }


    // Start typed Alternative
    trait AlternativeTyperEvent extends TyperEvent {
      override def tag = super.tag + "-alternative"
    }

    case class AlternativeTyper(alternative: Tree)
      extends TreeEvent with AlternativeTyperEvent {
      val tree = duplicateTreeWithPos(alternative)
    }


    // Start typed Star
    trait StarTyperEvent extends TyperEvent {
      override def tag = super.tag + "-star"
    }

    case class StarTyper(tree0: Tree)
      extends TreeEvent with StarTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }

    // Start typed Bind
    trait BindTyperEvent extends TyperEvent {
      override def tag = super.tag + "-bind"
    }

    case class BindTyper(body: Tree)
      extends TreeEvent with BindTyperEvent {
      val tree = duplicateTreeWithPos(body)
    }

    // Start typed UnApply
    trait UnApplyTyperEvent extends TyperEvent {
      override def tag = super.tag + "-unapply"
    }

    case class UnApplyTyper(fun: Tree, args0: List[Tree])
      extends TreeEvent with UnApplyTyperEvent {
      val tree = duplicateTreeWithPos(fun)
      val args = args0.map(duplicateTreeWithPos)
    }

    // Start typed ArrayValue
    trait ArrayValueTyperEvent extends TyperEvent {
      override def tag = super.tag + "-arrayvalue"
    }

    case class ArrayValueTyper(elemtpt: Tree, elems0: List[Tree])
      extends TreeEvent with ArrayValueTyperEvent {
      val tree = duplicateTreeWithPos(elemtpt)
      val elems = elems0.map(duplicateTreeWithPos)
    }

    // Start typed Function
    trait FunctionTyperEvent extends TyperEvent {
      override def tag = super.tag + "-function"
    }

    case class FunctionTyper(tree0: Tree, constr: Boolean)
      extends TreeEvent with FunctionTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }


    // Start typed Assign ------------------
    trait AssignTyperEvent extends TyperEvent {
      override def tag = super.tag + "-assign"
    }

    case class AssignTyper(value01: Tree, value02: Tree) extends BinaryEvent[Tree]
      with AssignTyperEvent {
      val value1 = duplicateTreeWithPos(value01)
      val value2 = duplicateTreeWithPos(value02)
      def binaryOp = "="
      override def eventString = eventStringRep
    }

    case class AssignLeftTyper(tree0: Tree, tpe: Type, treeSym: Symbol)
      extends TreeTypeEventUnary with AssignTyperEvent {
      override def tag = super.tag + "-left"
      val tree = duplicateTreeWithPos(tree0)
    }

    case class AssignGetterTyper(tree0: Tree, tpe: Type) extends TreeEvent
      with AssignTyperEvent {
      override def tag = super.tag + "-getter"
      val tree = duplicateTreeWithPos(tree0)
    }

    case class AssignRightTyper(tree0: Tree, tpe: Type) extends TreeEvent
      with AssignTyperEvent {
      override def tag = super.tag + "-right"
      val tree = duplicateTreeWithPos(tree0)
    }

    // If events   ---------------------
    trait IfTyperEvent extends TyperEvent {
      override def tag = super.tag + "-if"

    }

    case class IfTyper(cond0: Tree, thenp0: Tree, elsep0: Tree) extends Event
      with IfTyperEvent {
      protected def participants: List[Any] = List(cond, thenp, elsep)
      val (cond, thenp, elsep) = (duplicateTreeWithPos(cond0), duplicateTreeWithPos(thenp0), duplicateTreeWithPos(elsep0))
    }

    // value: current type of tree
    case class IfCondTyper(tree0: Tree)
      extends TreeEvent with IfTyperEvent {
      override def tag = super.tag + "-cond"
      val tree = duplicateTreeWithPos(tree0)
    }

    case class IfBranchTyper(tree0: Tree, pt: Type, cond: Boolean)
      extends TreeEvent with IfTyperEvent {

      override def tag = super.tag + "-" + branch
      def branch = if (cond) "then" else "else"
      val tree = duplicateTreeWithPos(tree0)
    }

    case class IfLubTyper(tree01: Tree, tree02: Tree, value1: Type, value2: Type, pt: Type, expected: Tuple2[Type, Boolean])
      extends TreeTypeEventBinary with IfTyperEvent {
      def binaryOp = "ptOrLub"
      override def tag = super.tag + "-ptOrLub"
      val tree1 = duplicateTreeWithPos(tree01)
      val tree2 = duplicateTreeWithPos(tree02)
    }

    // Start typed Match
    trait MatchTyperEvent extends TyperEvent {
      override def tag = super.tag + "-match"
    }

    case class MatchTyper(tree0: Tree)
      extends TreeEvent with MatchTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }

    // Start typed Return
    case class ReturnTyper(tree0: Tree, tpe: Type) extends TreeTypeEventUnary
      with AssignTyperEvent {
      override def tag = super.tag + "-return"
      val tree = duplicateTreeWithPos(tree0)
    }

    // Start typed Try
    trait TryTyperEvent extends TyperEvent {
      override def tag = super.tag + "-try"
    }

    case class TryTyper(block: Tree, catches0: List[Tree], fin0: Tree)
      extends TreeEvent with TryTyperEvent {
      val tree = duplicateTreeWithPos(block)
      val catches = catches0.map(duplicateTreeWithPos)
      val fin = duplicateTreeWithPos(fin0)
    }

    // Start typed Throw
    trait ThrowTyperEvent extends TyperEvent {
      override def tag = super.tag + "-throw"
    }

    case class ThrowTyper(tree0: Tree)
      extends TreeEvent with ThrowTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }

    // Start typed New
    trait NewTyperEvent extends TyperEvent {
      override def tag = super.tag + "-new"
    }

    case class NewTyper(tree0: Tree) extends TreeEvent
      with NewTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }

    case class NewTypeCtorTyper(tree0: Tree) extends TreeEvent
      with NewTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = super.tag + "-typeCtor"
    }

    case class NewTypeCtorWithParamsTyper(tree0: Tree, params: List[Symbol]) extends TreeEvent
      with NewTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = super.tag + "-typeCtorWithParams"
    }

    // Start typed Eta
    trait EtaTyperEvent extends TyperEvent {
      override def tag = super.tag + "-eta"
    }

    case class EtaTyper(tree0: Tree, tpe: Type, pt: Type)
      extends TreeTypeEventUnary with EtaTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }

    case class EtaByNameParamTyper(tree0: Tree, tpe: Type)
      extends TreeTypeEventUnary with EtaTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = super.tag + "-by-name"
    }

    case class EtaEmptyPolyTyper(tree0: Tree, tpe: Type) extends TreeTypeEventUnary
      with EtaTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
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

    case class EtaMethodTypeTyper(tree0: Tree, params: List[Symbol])
      extends TreeEvent with EtaTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = "perform-eta-expansion-adaption-for-method"
    }

    case class EtaMethodDoneTyper(tree0: Tree, tpe: Type)
      extends TreeTypeEventUnary with EtaTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = super.tag + "-method-done"
    }

    case class EtaMethodAdaptTyper(tree0: Tree, formals: List[Symbol])
      extends TreeEvent with EtaTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = super.tag + "-method-adapt"
    }

    case class TryTypedArgsTyper(args0: List[Tree], mode: Int)
      extends Event with TyperEvent {
      val args = args0.map(duplicateTreeWithPos)
      override def tag = "typecheck-all-arguments-separately"
      def participants = List(args, mode)
    }

    // Start tryTypedApply

    trait TryTypedApplyEvent extends TyperEvent {
      override def tag = super.tag + "-try-typed-apply"
    }

    case class TryTypedApplyTyper(fun0: Tree, args0: List[Tree], pt: Type)
      extends TreeEvent with TreeReferencesEvent with TryTypedApplyEvent {
      override def tag = "try-typechecking-arguments-application-to-function"
      val tree = duplicateTreeWithPos(fun0)
      val args = args0.map(duplicateTreeWithPos)
      def references = args
    }

    case class SuccessTryTypedApplyTyper(tree0: Tree, tpe: Type, pt: Type) extends TreeTypeEventUnary
      with TryTypedApplyEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = "successfully-typed-application"
    }

    case class SecondTryTypedApplyStartTyper(
      fun: Tree, args: List[Tree], errorMsg: String, pt: Type)
      extends TreeEvent with TryTypedApplyEvent {

      val tree = duplicateTreeWithPos(fun)
      override def tag = "second-attempt:adapt-function-to-arguments"
    }

    case class SecondTryTypedApplyTyper(tree0: Tree, tpe: Type, args1: List[Tree], qual1: Tree, pt: Type) extends TreeEvent
      with TryTypedApplyEvent {

      val tree = duplicateTreeWithPos(tree0)
      override def tag = "second-applying-arguments-to-function"
    }

    case class FailedSecondTryTypedApplyTyper(fun: Tree, args1: List[Tree], pt: Type) extends TreeEvent with TryTypedApplyEvent with HardErrorEvent {
      val tree = duplicateTreeWithPos(fun)
      override def tag = "failed-adapting-qualifier-to-arguments"
    }

    case class FailedTryTypedApplyTyper(fun: Tree, args: List[Tree], pt: Type) extends Event
      with TryTypedApplyEvent with HardErrorEvent {
      val tree = duplicateTreeWithPos(fun)
      override def tag = super.tag + "-failed-try"
      protected def participants: List[Any] = List(fun, args, pt)
    }

    // Start typed Typed with wildcard
    trait TypedWildcardTyperEvent extends TyperEvent {
      override def tag = super.tag + "-typedwildcard"
    }

    case class TypedWildcardTyper(tree0: Tree)
      extends TreeEvent with TypedWildcardTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }

    // Start typed Typed
    trait TypedTyperEvent extends TyperEvent {
      override def tag = super.tag + "-typed"
    }

    case class TypedEtaTyper(tree0: Tree, expr: Tree)
      extends TreeEvent with TypedTyperEvent {
      val tree = duplicateTreeWithPos(expr)
      override def tag = "typing-expr-to-be-eta-expanded"
    }

    case class TypedTypedExprTyper(tree0: Tree)
      extends TreeEvent with TypedTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = super.tag + "-general"
    }

    // Start typed Apply
    trait TypedApplyEvent extends TyperEvent {
      override def tag = super.tag + "-typed-apply"
    }

    // TODO: positions
    case class TypedApplyTyper(fun0: Tree, args0: List[Tree], pt: Type)
      extends Event with TypedApplyEvent {
      val fun = duplicateTreeWithPos(fun0)
      val args = args0.map(duplicateTreeWithPos)
      protected def participants: List[Any] = List(fun0, args0, pt)
      override def tag = "type-type-application"
    }

    // tree only for positions
    case class TypedTypeApplySuccessTyper(tree0: Tree, fun0: Tree, args0: List[Tree], resultTpe: Type)
      extends TreeEvent with TypedApplyEvent {
      val tree = duplicateTreeWithPos(fun0)
      val args = args0.map(duplicateTreeWithPos)
      def fun = tree
      override def tag = super.tag + "-success"
    }

    case class TypedApplyStableTyper(tree0: Tree, pt: Type)
      extends TreeEvent with TypedApplyEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = super.tag + "-stable"
    }

    case class TypedApplyUnstableTyper(tree0: Tree, patternMode: Boolean, pt: Type, args0: List[Tree])
      extends TreeEvent with TypedApplyEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = "unstable-function"
    }

    case class SuccessTypedApplyFunTyper(tree0: Tree, expectedFunPt: Type, stableApplication: Boolean, pt: Type)
      extends TreeEvent with TypedApplyEvent with SymbolReferencesEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = "successfully-typed-function"

      def references = if (tree.symbol != null && tree.symbol.isImplicit && tree.pos == NoPosition) {
        List(tree.symbol)
      } else List()
    }

    case class TypedApplyTryTyper(fun0: Tree, args0: List[Tree])
      extends Event with TypedApplyEvent {
      override def tag = super.tag + "-try-typed-apply"
      protected def participants: List[Any] = List(fun0, args0)
    }

    case class TypedApplyForceTyper(fun0: Tree, args0: List[Tree])
      extends Event with TypedApplyEvent {
      override def tag = super.tag + "-force-typed-apply"
      protected def participants: List[Any] = List(fun0, args0)
    }

    case class TypedApplyArrayApplyTyper(fun0: Tree, checkedTree: Boolean)
      extends Event with TypedApplyEvent {
      override def tag = super.tag + "-array-apply"
      protected def participants: List[Any] = List(fun0, checkedTree)
    }

    case class TypedApplyErrorTyper(fun0: Tree)
      extends Event with TypedApplyEvent with SoftErrorEvent {
      val fun = duplicateTreeWithPos(fun0)
      override def tag = super.tag + "-error"
      protected def participants: List[Any] = List(fun0)
    }

    case class TypedApplyQualifierTyper(qual0: Tree, name: Name, pt: Type)
      extends Event with TypedApplyEvent {
      override def tag = super.tag + "-assignment-op"
      protected def participants: List[Any] = List(qual0, name, pt)
    }

    case class TypedApplyToAssignment(fun0: Tree, qual0: Tree, name: Name, args0: List[Tree])
      extends Event with TypedApplyEvent {
      override def tag = super.tag + "-convert-to-assignment"
      protected def participants: List[Any] = List(fun0, qual0, name, args0)
    }

    // Start typed ApplyBlock
    trait ApplyBlockTyperEvent extends TyperEvent {
      override def tag = super.tag + "-applyblock"
    }

    case class ApplyBlockTyper(tree0: Tree)
      extends TreeEvent with ApplyBlockTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }

    // Start typed Apply
    trait ApplyTyperEvent extends TyperEvent {
      override def tag = super.tag + "-apply"
    }

    case class ApplyTyper(app: Apply, e: Explanation)
      extends TreeEvent with ApplyTyperEvent with SymbolReferencesEvent {
      override def tag = "type-application"
      val tree = duplicateTreeWithPos(app)
      def references = e match {
//        case TypeImplicitApplication(_, _, sym) =>
//          List(sym)
        case _ => tree.asInstanceOf[Apply].args.map(_.symbol).filter(_ != NoSymbol)
      }
    }

    // Start typed ApplyDynamic
    trait ApplyDynamicTyperEvent extends TyperEvent {
      override def tag = super.tag + "-applydynamic"
    }

    case class ApplyDynamicTyper(tree0: Tree)
      extends TreeEvent with ApplyDynamicTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }

    // Start typed Super
    trait SuperTyperEvent extends TyperEvent {
      override def tag = super.tag + "-super"
    }

    case class SuperTyper(qual0: Tree)
      extends TreeEvent with SuperTyperEvent {
      val tree = duplicateTreeWithPos(qual0)
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

    case class SelectTyper(qual0: Tree, name: Name, origTree: Tree, pt: Type)
      extends TreeEvent with SelectTyperEvent {
      val tree = duplicateTreeWithPos(origTree)
      val qual = duplicateTreeWithPos(qual0)
      override def tag = "type-full-selection"
    }

    case class SelectTreeTyper(tree0: Tree, name: Name, pt: Type, constr: Boolean)
      extends TreeEvent with SelectTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = if (constr) "type-new-object" else (super.tag + "-tree")
    }

    case class SelectConstrTyper(qual0: Tree)
      extends TreeEvent with SelectTyperEvent {
      val tree = duplicateTreeWithPos(qual0)
      override def tag = "type-super-constructor-call"
    }

    case class TreeSymSelectTyper(sym: Symbol)
      extends SymEvent with SelectTyperEvent with DebugEvent {
      override def tag = super.tag + "-qualSym"
    }

    case class SymSelectTyper(qual0: Tree, member: Name, sym: Symbol)
      extends TreeEvent with SelectTyperEvent {
      override def tag = (if (sym == NoSymbol) "qualifier-without-symbol" else "qualifier-with-symbol")
      val tree = duplicateTreeWithPos(qual0)
      def qual = tree
    }

    case class SymExistsSelectTyper(sym: Symbol)
      extends SymEvent with SelectTyperEvent {
      override def tag = super.tag + "-sym-exists"
    }

    case class StabilizeTreeTyper(tree0: Tree, sym: Symbol, tpe: Type)
      extends TreeEvent with SelectTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
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

    case class IdentTyper(tree0: Ident)
      extends TreeEvent with IdentTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }

    // Start typed Literal
    trait LiteralTyperEvent extends TyperEvent {
      override def tag = super.tag + "-literal"
    }

    case class LiteralTyper(tree0: Literal, pt: Type)
      extends TreeEvent with LiteralTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }


    // Start typed SingletonTypeTree
    trait SingletonTypeTreeTyperEvent extends TyperEvent {
      override def tag = super.tag + "-singleton"
    }

    case class SingletonTypeTreeTyper(tree0: Tree)
      extends TreeEvent with SingletonTypeTreeTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }


    // Start typed SelectFromTypeTree
    trait SelectFromTypeTreeTyperEvent extends TyperEvent {
      override def tag = super.tag + "-selecttypetree"
    }

    case class SelectFromTypeTreeTyper(qual0: Tree, select: Name)
      extends TreeEvent with SelectFromTypeTreeTyperEvent {
      val tree = duplicateTreeWithPos(qual0)
    }


    // Start typed CompoundTypeTree
    trait CompoundTypeTyperEvent extends TyperEvent {
      override def tag = super.tag + "-compoundtype"
    }

    case class CompoundTypeTyper(tree0: Tree)
      extends TreeEvent with CompoundTypeTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }


    // Start typed AppliedTypeTree
    trait AppliedTypeTyperEvent extends TyperEvent {
      override def tag = super.tag + "-appliedtype"
    }

    case class AppliedTypeTyper(tree0: Tree, args0: List[Tree])
      extends TreeEvent with AppliedTypeTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
      val args = args0.map(duplicateTreeWithPos)
      override def tag = "type-applied-type"
    }


    // Start typed TypeBoundsTree
    trait TypeBoundsTyperEvent extends TyperEvent {
      override def tag = super.tag + "-type-bounds"
    }

    // TODO: there seems to be a bug related to range positions, hence
    // def foo[T] will not show correctly the position for T
    case class TypeBoundsTyper(bounds0: Tree, treel: Tree, treeh: Tree)
      extends TreeEvent with TypeBoundsTyperEvent {
      val tree = duplicateTreeWithPos(bounds0)
      def bounds = tree
      override def participants = List(treel, treeh)
    }


    // Start typed ExistentialType
    trait ExistentialTypeTyperEvent extends TyperEvent {
      override def tag = super.tag + "-existential"
    }

    case class ExistentialTypeTyper(tree0: Tree)
      extends TreeEvent with ExistentialTypeTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }


    // Start typed DeferredRefCheckType
    trait DeferredRefCheckTypeTyperEvent extends TyperEvent {
      override def tag = super.tag + "-deferredrefcheck"
    }

    case class DeferredRefCheckTypeTyper(tree0: Tree)
      extends TreeEvent with DeferredRefCheckTypeTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }


    // Start typed TypeTree
    trait TypeTreeTyperEvent extends TyperEvent {
      override def tag = super.tag + "-typetree"
    }

    case class TypeTreeTyper(tree0: Tree)
      extends TreeEvent with TypeTreeTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }


    // Start typed Import
    trait ImportTyperEvent extends TyperEvent {
      override def tag = super.tag + "-import"
    }

    case class ImportTyper(tree0: Tree, selectors: List[ImportSelector])
      extends TreeEvent with ImportTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }


    // Start typed Unexpected
    trait UnexpectedTyperEvent extends TyperEvent {
      override def tag = super.tag + "-unexpected"
    }

    case class UnexpectedTyper(tree0: Tree)
      extends TreeEvent with UnexpectedTyperEvent {
      val tree = duplicateTreeWithPos(tree0)
    }


    trait TemplateTyperEvent extends TyperEvent {
      override def tag = super.tag + "-template"
    }

    case class TemplateTyper(templ0: Template, parents0: List[Tree],
        clazz: Symbol)(implicit val info: TemplateInfo)
      extends TreeEvent with TemplateTyperEvent {
      val tree = duplicateTreeWithPos(templ0)
      def templ = tree
      override def lName =
        info match {
          case ClassTemplate  => "Type class template"
          case ModuleTemplate => "Type object template"
          case TraitTemplate => "Type trait template"
        }
      override def participants = List(tree, parents)
      val parents = parents0.map(duplicateTreeWithPos)
    }

    case class SelfTpeRefinedType(parents0: List[Type], owner: Symbol)
      extends Event with TemplateTyperEvent {
      override def tag = super.tag + "-refined-self-type"
      override def participants = parents0
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

    case class AdaptToArgumentsTyper(tree0: Tree, name: Name, args0: List[Tree], pt: Type)
      extends TreeEvent with AdaptToArgumentsEvent {
      val tree = duplicateTreeWithPos(tree0)
      val args = args0.map(duplicateTreeWithPos)
      override def tag = "adapt-function-to-arguments"
    }

    case class FinishedAdaptToArgumentsTyper(value01: Tree, value02: Tree,
        isWildcard: Boolean, originEvent: Int)
      extends TwoTreeEvent with AdaptToArgumentsEvent with DoneBlock {
      val value1 = duplicateTreeWithPos(value01)
      val value2 = duplicateTreeWithPos(value02)
      override def tag =
        (if (value1 eq value2) "failed-to-adapt-function-to-arguments"
        else "successfully-adapted-function-to-arguments")
      def binaryOp = ""
    }

    case class FallbackAdaptToArgumentsTyper(tree0: Tree, error: String)
      extends TreeEvent with AdaptToArgumentsEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = "fallback:try-to-adapt-with-no-expected-type"
    }

    trait AdaptToMemberEvent extends AdaptToEvent {
      def tag = "adaptToMember"
    }

    case class AdaptToMemberTyper(tree0: Tree, tpe: Type, searchTpe: Type)
      extends TreeEvent with AdaptToMemberEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = "adapt-to-member"
    }

    case class OpenExistentialAdaptToMemberTyper(tpe: Type)
      extends TypeEvent with AdaptToMemberEvent {
      override def tag = super.tag + "-openExistential"
    }

    // todo obsolete?
    case class FoundCoercionAdapToMemberTyper(qual0: Tree, coercion: Tree)
      extends TreeEvent with TreeReferencesEvent with AdaptToMemberEvent {
      val tree = duplicateTreeWithPos(qual0)
      override def tag = super.tag + "-found-coercion"
      def references = List(coercion)
    }

    case class FailedAdaptToMemberTyper(tree0: Tree, tpe: Type, searchTpe: Type, originEvent: Int)
      extends TreeEvent with AdaptToMemberEvent
      with SoftErrorEvent with DoneBlock {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = super.tag + "-failed"
    }

    case class IsNotAdaptableTyper(tree0: Tree, tpe: Type)
      extends TreeEvent with AdaptToMemberEvent with SoftErrorEvent {
      val tree = duplicateTreeWithPos(tree0)
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

    case class DoTypedApplyTyper(tree0: Tree, fun0: Tree, args0: List[Tree], mode: Int, pt: Type)
    extends TreeEvent with TreeReferencesEvent with DoTypedApplyEvent {
      val tree = duplicateTreeWithPos(fun0)
      val args = args0.map(duplicateTreeWithPos)
      def references = args
      def fun = tree
      override def tag = "typecheck-arguments-application-to-function"
    }

    // overloaded sym
    case class OverloadedSymDoTypedApply(tree0: Tree, sym: Symbol,
        argTypes: List[Type], pre: Type)
      extends TreeEvent with DoTypedApplyEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = "quick-alternatives-filter-for-overloaded-function"
    }

    case class CheckApplicabilityAlternativeDoTypedApply(
      tree0: Tree, funSym: Symbol, pt: Type, alt: Symbol)
      extends TreeEvent with DoTypedApplyEvent with SymbolReferencesEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = super.tag + "-check-alternative-symbol"
      def sym = funSym
      def references = List(alt)
    }

    case class IsApplicableAlternativeDoTypedApply(sym: Symbol,
        ftpe: Type, originEvent: Int, applicable: Boolean)
      extends SymEvent with DoTypedApplyEvent with DoneBlock {
      override def tag = super.tag + "-overloadedSym-applicable"
    }

    case class FilteredDoTypedApply(tree0: Tree, funSym: Symbol, originEvent: Int)
      extends TreeEvent with DoTypedApplyEvent with DoneBlock {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = "filtered-out-alternatives"
    }

    case class OverloadedTpeDoTypedApply(fun0: Tree, pre: Type, alts: List[Symbol])
      extends Event with DoTypedApplyEvent with SymbolReferencesEvent {
      val fun = duplicateTreeWithPos(fun0)
      override def tag = "typecheck-application-for-overloaded-method"
      def participants = alts
      def references = alts
    }

    case class InferMethodAlternativeDoTypedApply(fun0: Tree)
      extends TreeEvent with DoTypedApplyEvent {
      val tree = duplicateTreeWithPos(fun0)
      override def tag = "infer-correct-method-for-application-from-alternatives-"
    }

    case class AdaptInferredMethodAlternativeDoTypedApply(fun0: Tree)
      extends TreeEvent with DoTypedApplyEvent with SymbolReferencesEvent {
      val tree = duplicateTreeWithPos(fun0)
      def fun = fun0
      override def tag = "adapt-inferred-method-alternative"
      def references = List(tree.symbol)
    }

    case class InferredMethodDoTypedApply(fun0: Tree)
      extends TreeEvent with DoTypedApplyEvent {
      val tree = duplicateTreeWithPos(fun0)
      def fun = tree
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

    case class PackArgsDoTypedApply(tupleArgs0: List[Tree])
      extends Event with DoTypedApplyEvent {
      override def tag = super.tag + "-tuple-packed"
      def participants = List(tupleArgs0)
    }

    case class PackedArgsDoTypedApply(tree0: Any, originEvent: Int, status0: Boolean)
      extends Event with DoTypedApplyEvent with DoneBlock {
      override def tag = super.tag + "-packedArgs[" + (if (status) "OK" else "FAILED") + "]"
      override def status = status0
      def participants = List(tree0)
    }

    case class TryNamesDefaultsDoTypedApply(args0: List[Tree], formals: List[Type])
      extends Event with DoTypedApplyEvent {
      override def tag = super.tag + "-tryNamesDefaults"
      def participants = List(args0, formals)
    }

    case class CorrectArgumentsDoTypedApply(tree0: Tree, formals: List[Type], args: List[Tree])
      extends TreeEvent with DoTypedApplyEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = "correct-number-of-arguments"
    }

    case class TParamsResolvedDoTypedApply(tree0: Tree)
      extends TreeEvent with DoTypedApplyEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = "all-type-parameters-are-resolved"
    }

    case class TypedArgsDoTypedApply(args0: List[Tree], originEvent: Int)
      extends Event with DoTypedApplyEvent with DoneBlock {
      def participants = List(args0)
      override def tag = "arguments-successfully-typed"
    }

    case class ApplyTreeDoneDoTypedApply(tree0: Tree, originEvent: Int)
      extends TreeEvent with DoTypedApplyEvent with DoneBlock {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = "successfully-typed-application"
    }

    case class NeedsInstantiationDoTypedApply(tparams: List[Symbol], formals: List[Type], args0: List[Tree])
      extends Event with DoTypedApplyEvent {
      override def tag = super.tag + "-needsInstantiation"
      def participants = List(tparams)
    }

    case class MethodTpeWithUndetTpeParamsDoTypedApply(fun0: Tree, tparams: List[Symbol],
      formals: List[Type]) extends TreeEvent with DoTypedApplyEvent {
      val tree = duplicateTreeWithPos(fun0)
      override def tag = "method-with-undetermined-type-parameters"
    }

     case class ProtoTypeArgsDoTypedApply(tparams: List[Symbol], formals: List[Type],
      resultTpe: Type, lenient: List[Type], pt: Type)
      extends Event with DoTypedApplyEvent {
      override def tag = super.tag + "-prototypeargs"
      def participants = tparams
    }

    case class InstantiatedDoTypedApply(fun0: Tree, pt: Type, undet: List[Symbol])
      extends TreeEvent with DoTypedApplyEvent {
      val tree = duplicateTreeWithPos(fun0)
      def fun = tree
      override def tag = "typecheck-inferred-instance-in-the-application"
    }

    case class DoTypedApplyDone(originEvent: Int, tree0: Tree)
      extends TreeEvent with DoTypedApplyEvent with DoneBlock {
      val tree = duplicateTreeWithPos(tree0)

      override def tag = super.tag + "-done"
    }

    case class SingleTpeDoTypedApply(tpe: Type)
      extends TypeEvent with DoTypedApplyEvent {
      override def tag = super.tag + "-singleTpe"
    }

    case class ErrorTpeDoTypedApply(tree0: Tree)
      extends TreeEvent with DoTypedApplyEvent {
      val tree = duplicateTreeWithPos(tree0)
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

    // Information
    trait TemplateInfo

    case object ClassTemplate extends TemplateInfo
    case object ModuleTemplate extends TemplateInfo
    case object TraitTemplate extends TemplateInfo

    //Package
    case class TypePackageQualifier(qual: RefTree)
      extends Explanation with TyperExplanation

    case class TypePackageStatement(stat: Tree)
      extends Explanation with TyperExplanation

    // Template
    case class TypeTemplateStatement(stat: Tree)(implicit info: TemplateInfo)
      extends Explanation with TyperExplanation {
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
    case class TypeFunctionTypeApply(fun: Tree)
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
    case class TypeArgsStandalone(args: List[Tree])
      extends Explanation with TyperExplanation {

      override def provide(a: Tree): Explanation = TypeArgStandalone(a)
    }

    //Do typed apply, try typed apply
    case class TypeArgStandalone(arg: Tree)
      extends Explanation with TyperExplanation

    //Block
    case class TypeStatementInBlock(stat: Tree)(implicit underlying0: Explanation)
      extends Explanation with TyperExplanation {

      override def underlying = underlying0
    }

    case class TypeLastStatementInBlock(stat: Tree)(implicit underlying0: Explanation)
      extends Explanation with TyperExplanation {

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
    case class TypeValType(tpt: Tree)
      extends Explanation with TyperExplanation

    //Type constructor
    case class TypeTypeConstructor(tree: Tree)
      extends Explanation with TyperExplanation

    case class TypeTypeConstructorInNew(tpt: Tree)
      extends Explanation with TyperExplanation

    //Parent typing

    case class TypeInitialSuperType(supertpt0: Tree)
      extends Explanation with TyperExplanation {
      val supertpt = duplicateTreeWithPos(supertpt0)
    }


    case class TypeParentMixin(mixin0: Tree)
      extends Explanation with TyperExplanation {
      val mixin = duplicateTreeWithPos(mixin0)
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
    case class TypeRefinementStatement(tree: Tree)
      extends Explanation with TyperExplanation

    case class TypeExistentialTypeClause(tree: Tree)
      extends Explanation with TyperExplanation

    // Arg
    case class TypeArgForCorrectArgsNum(arg: Tree)
      extends Explanation with TyperExplanation

    case class TypeArgLenientPt(arg: Tree)
      extends Explanation with TyperExplanation


    // Use case?
    case class TypeUseCaseStatement(tree: Tree)
      extends Explanation with TyperExplanation


    // Other [adapt]
    case class AdaptQualToMemberInferView(qual: Tree, to: Type)
      extends Explanation with TyperExplanation
  }
}