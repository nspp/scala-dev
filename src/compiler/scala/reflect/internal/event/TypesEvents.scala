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

    // TODO copy types?
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

    
    object Side extends Enumeration {
      val Left, Right, Both, Other = Value
    }
    
    object SubCompare extends Enumeration {
      val CTypeRef, CAnnotated, CSingletonClass, CClassSym, CSkolemizedExist,
      CRefined, CNullary, CTypeBounds, CMethod,
      COther = Value // only used for failed comparisons
    } 
    
    import Side._ 
    
    // All events involved in subtyping tests
    trait Subtyping {
      val tp1: Type
      val tp2: Type
      val compType: SubCompare.Value
      val which: Side.Value
      def participants = List()
    }
        
    case class CompareTypes(tp10: Type, tp20: Type, which: Side.Value, compType: SubCompare.Value)
      extends Event with TypesEvent with Subtyping {
      val tp1 = deepTypeClone(tp10)
      val tp2 = deepTypeClone(tp20)
    }

    case class CompareDone(tp1: Type, tp2: Type, originEvent: Int, subtypes: Boolean)
      extends Event with TypesEvent with Subtyping with DoneBlock {
      val which = Other
      val compType = SubCompare.COther
    }
    
    case class FailedSubtyping(tp1: Type, tp2: Type, which: Side.Value, compType: SubCompare.Value)
      extends Event with TypesEvent with Subtyping {
    }
  }

  
  trait TypesExplanations {
    self: EventModel =>

    trait TypesExplanation
    
    case class SubTypeArgs extends Explanation with TypesExplanation
  }

}