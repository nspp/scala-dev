package scala.tools.nsc
package event

import typechecker.Analyzer

trait GeneralEvents {
  self: symtab.SymbolTable with EventsSymbolTable =>

  trait ControlEvents {
    self: EventModel =>

    case class NewParse(tree: Tree) extends TreeEvent {
      def tag = "newParse"
    }
    case class ThisPhaseDone(override val unit: CompilationUnit) extends Event {
      def tag = "phaseDone"
      protected def participants: List[Any] = List(unit)
    }
    case class NewScope(scope: Scope) extends Event {
      def tag = "newScope"
      protected def participants: List[Any] = List(scope)
    }
    case class NewBaseTypeSeq(bts: BaseTypeSeq) extends Event {
      def tag = "newBaseTypeSeq"
      protected def participants: List[Any] = List(bts)
    }
  }

  trait ErrorEvents {
    self: EventModel =>

    // merge with soft/hard error
    object ErrorLevel extends Enumeration {
      val Soft, Hard = Value
    }

    trait ContextTypeError {
      def err: Analyzer#AbsTypeError
      def participants: List[Any] = List(err)
      def errType: ErrorLevel.Value
    }

    case class ContextAmbiguousTypeErrorEvent(err: Analyzer#AbsTypeError, errType: ErrorLevel.Value)
      extends Event with ContextTypeError {
      def tag = "context-ambiguous-error"
    }

    case class ContextTypeErrorEvent(err: Analyzer#AbsTypeError, errType: ErrorLevel.Value)
      extends Event with ContextTypeError {
      def tag = "context-error"
    }
  }
}