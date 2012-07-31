package scala.reflect
package internal
package event

trait TypesEventsUniverse {
  out: SymbolTable with EventsUniverse =>

  trait TypesEvents {
    self: EventModel =>
      
    import Util._

    trait TypesEvent {
      def tag = "types"
    }
    
    // TODO remove
    case class TypesDone(originEvent: Int) extends Event with TypesEvent with DoneBlock {
      override def tag = super.tag + "-done"
      def participants = List()
    }

    case class SubTypeCheck(value1: Type, value2: Type) extends TwoTypeEvent with TypesEvent {
      override def tag = super.tag + "-subTypeCheck"
      def lhs      = value1
      def rhs      = value2
      def binaryOp = "<:<"
    }
    
    case class SubTypeCheckRes(originEvent: Int, res: Boolean) extends Event with TypesEvent with DoneBlock {
      override def tag = super.tag + "-subtypecheck-done"
      def participants = List(res)
    }
    
    case class SubTypeCheckArg(variance: Int) extends Event with TypesEvent {
      override def tag = super.tag + "-subTypeCheck-arg"
      def participants = List()
    }
    
    case class IsWithinBounds(tp: Type, tconstr: TypeConstraint) extends Event with TypesEvent {
      override def tag = super.tag + "-istype-within-constraints"
      def participants = List(tconstr)
    }
    
    case class IsTArgWithinBounds(bounds: TypeBounds, tparam: Symbol, targ: Type) extends Event with TypesEvent {
      override def tag = super.tag + "-istype-within-bounds"
      def this(boundAndTParam: Tuple2[TypeBounds, Symbol], targ: Type) = this(boundAndTParam._1, boundAndTParam._2, targ)
      def participants = List(bounds, targ)
    }
    
    object IsTArgWithinBounds {
      def apply(boundAndTParam: Tuple2[TypeBounds, Symbol], targ: Type): IsTArgWithinBounds = IsTArgWithinBounds(boundAndTParam._1, boundAndTParam._2, targ)
    }
        
    
    case class TArgWithinBoundsDone(originEvent: Int, res: Boolean) extends Event with TypesEvent with DoneBlock {
      override def tag = super.tag + "-istype-within-bounds-done"
      def participants = List()      
    }
    
    // TODO: remove
    case class RegisterBound(tvar: TypeVar, bound: Type, isLowerBound: Boolean)
      extends Event with TypesEvent {
      def participants = List(tvar)
    }

    
    object Side extends Enumeration {
      val Left, Right, Both, Other = Value
    }
    
    object SubCompare extends Enumeration {
      val CTypeRef, CTypeRefBase, CAnnotated, CSingletonClass, CClassSym, CSkolemizedExist,
      CRefined, CNullary, CTypeBounds, CMethod,
      CClassSymRaw, CClassSymRefined,
      CTypeSymDeferred, CTypeSymNonDeferred,
      CSingletonOrNotNull,
      CHigherKindedParams, CHigherKindedRes, 
      CHigherKindedTpe,
      COther = Value // only used for failed comparisons
    }
    
    import Side._ 
    
    // All events involved in subtyping tests
    trait Subtyping {
      def tp1: Type
      def tp2: Type
      def compType: SubCompare.Value
      def which: Side.Value
      def participants = List()
    }

    case class CompareTypes(tp1: Type, tp2: Type, which: Side.Value, compType: SubCompare.Value)
      extends Event with TypesEvent with Subtyping {
    }

    case class CompareDone(tp1: Type, tp2: Type, originEvent: Int, subtypes: Boolean)
      extends Event with TypesEvent with Subtyping with DoneBlock {
      def which = Other
      def compType = SubCompare.COther
    }
    
    case class FailedSubtyping(tp1: Type, tp2: Type, which: Side.Value, compType: SubCompare.Value)
      extends Event with TypesEvent with Subtyping {
    }
    
    case class NothingSubtype(tp2: Type)
      extends Event with TypesEvent with Subtyping {
      def tp1 = definitions.NothingClass.tpe
      def which: Side.Value = Left
      def compType = SubCompare.COther
    }
    
    // other
    case class InstantiateTypeParams(formals: List[Symbol], actuals: List[Type])
      extends Event with TypesEvent {
      def participants = formals
    }
  }

  
  trait TypesExplanations {
    self: EventModel =>

    trait TypesExplanation
    
    case class SubTypeArgs extends Explanation with TypesExplanation
  }

}