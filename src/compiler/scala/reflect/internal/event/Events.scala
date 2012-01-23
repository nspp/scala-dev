package scala.reflect
package internal
package event

trait Events {
  outer: SymbolTable with EventsUniverse =>

  trait AllEvents extends AnyRef
                  with BasicEvents
                  with AdaptEvents
                  with NamerEvents
                  with TyperEvents
                  with InferEvents
                  with UtilEvents
                  with ImplicitEvents {
    self: EventModel =>

    trait DoneBlock {
      def status: Boolean = true
      def originEvent: Int
    }

  }

  trait AllExplanations extends AnyRef
                        with AdaptExplanations
                        with NamerExplanations
                        with TyperExplanations
                        with InferExplanations
                        with UtilExplanations {
    self: EventModel =>

    trait Explanation {
      def descrName: String = ""
      def provide(a: Tree): Explanation = this
      override def toString = descrName
      def underlying: Explanation = this
    }

    case object DefaultExplanation extends Explanation {
      def descr: String = "No explanation"
      override def descrName: String = "no-explanation"
    }
  }


  trait BasicEvents {
    outer: EventModel =>

    case class SetFlag(sym: Symbol, oldFlags: Long, newFlags: Long, mask: Long) extends SymFlagEvent {
      def tag = "setFlag"
      override def eventString = joinString(sym, "+" + flagsString(mask))
    }
    case class ClearFlag(sym: Symbol, oldFlags: Long, newFlags: Long, mask: Long) extends SymFlagEvent {
      def tag = "clearFlag"
      override def eventString = joinString(sym, "-" + flagsString(mask))
    }
    case class SetModFlag(mods: Modifiers, oldFlags: Long, newFlags: Long, mask: Long) extends ModsFlagEvent {
      def tag = "setModFlag"
    }
    case class ClearModFlag(mods: Modifiers, oldFlags: Long, newFlags: Long, mask: Long) extends ModsFlagEvent {
      def tag = "clearModFlag"
    }
    case class NewTree(tree: Tree) extends TreeEvent {
      def tag = "newTree"
    }
    case class NewSym(sym: Symbol) extends SymEvent {
      def tag = "newSym"
    }
    case class CallMethod(value1: Symbol, value2: Symbol) extends TwoSymEvent {
      def tag          = "call"
      def caller       = value1.enclClass
      def callee       = value2.enclClass
      def sourceMethod = value1
      def targetMethod = value2
      // def exprType     = value2.tpe.resultType
      def binaryOp     = "->"

      private def callString(m: Symbol) = anyString(m.enclClass) + "." + anyString(m)
      override def eventString = joinString(callString(sourceMethod), binaryOp, callString(targetMethod))
    }
    case class SubTypeCheck(value1: Type, value2: Type, result: Boolean) extends TwoTypeEvent {
      def tag      = "subTypeCheck"
      def lhs      = value1
      def rhs      = value2
      def binaryOp = "<:<"
    }
    case class SymSetInfo(sym: Symbol, info: Type) extends SymEvent {
      def tag = "symSetInfo"
      override def eventString = {
        val elem = if (info.isComplete) info else "<incomplete>"
        joinString(sym, "set info", elem)
      }
    }

    case class NewTermSymbol(sym: TermSymbol) extends SymEvent {
      def tag = "newTermSymbol"
    }
    case class NewTypeSymbol(sym: TypeSymbol) extends SymEvent {
      def tag = "newTypeSymbol"
    }

    trait ErrorEvent {
      def pos0: Position
      def msg: String
      def participants: List[Any] = List(msg)
      protected def standardMsg = "[Error at " + pos0 + "] " + msg
      def tag = "error"
    }

    trait SoftErrorEvent
    trait HardErrorEvent
  }

  trait UtilEvents {
    self: EventModel =>



    /*
     * Since we have no way of guarding against the cyclic reference errors
     * (even the recoverable ones atm) Every catch _: TypeError has to issue this
     * event. Try has to start with an opening block event that is later given
     * as an argument.
     * This way, on the application level it is possible to implement logic
     * that would handle opening/closing the block information about events.
     * Atm there are no TypeErrors thrown directly, but there is no chance
     * to get rid off the cyclic references.
     *
     * There doesn't seem to be other way, but this overhead is acceptable once
     * we reduced the number of thrown errors.
     */

    trait RecoveryEvent

    case class ExceptionRecoveryEvent(lastOpen: Event) extends Event with RecoveryEvent {
      def participants = List(lastOpen)
      override def eventString = "Recovering from an exception thrown inside the compiler"
      def tag = "util-exception-recovery"
    }

    case class StartTryCatchBlock() extends Event with RecoveryEvent {
      def participants = List()
      override def eventString = "Open try/catch"
      def tag = "util-try-catch-open"
    }

    case class TryCatchFinally(originEvent: Int) extends Event with RecoveryEvent with DoneBlock {
      def participants = List()
      override def eventString = "Open try/catch"
      def tag = "util-try-catch-close"
    }

    // TODO REMOVE?
    case class InformativeEvent(underlying: Event) extends Event {
      def participants = List()
      override def eventString = underlying.eventString
      def tag = underlying.tag
    }

    trait DebugEvent
  }

  trait UtilExplanations {
    self: EventModel =>

    trait TreeInfo {
      def tree: Tree
    }

    trait RefsInfo[T] {
      def refs: List[T]
    }

    trait SymRefsInfo extends RefsInfo[Symbol]
    trait TreeRefsInfo extends RefsInfo[Tree]
  }

  object Util {
    @inline def withFullClone[T](f: => T): T = {
      val clone = settings.YfullClone.value // TODO use internal flag
      settings.YfullClone.value = true
      val res = f
      settings.YfullClone.value = clone
      res
    }

    def deepTypeClone[T <: Type](tpe: T): T = withFullClone {
      tpe.cloneInfo(if (tpe.typeSymbol == NoSymbol) NoSymbol else tpe.typeSymbol.owner).asInstanceOf[T]
    }

    def deepSymClone[T <: Symbol](sym: T): T = withFullClone {
      sym.cloneSymbol.asInstanceOf[T]
    }
  }
}