package scala.reflect
package internal
package event

trait NamerEventsUniverse {
  out: SymbolTable with EventsUniverse =>

  trait NamerEvents {
    self: EventModel =>
      
    import Util._

    trait NamerEvent {
      def tag = "namer"
    }

    case class NamerDone(originEvent: Int) extends Event with NamerEvent with DoneBlock {
      override def tag = super.tag + "-done"
      def participants: List[Any] = List()
    }

    // TODO: get rid off that one?
    case class TypeSigNamer(tree: Tree)
      extends TreeEvent with NamerEvent {
      override def tag = "type-signature-completer"
    }

    case class ClassSigNamer(templ: Template, tparams: List[TypeDef])
      extends TreeEvent with NamerEvent {
      def tree = templ
      override def tag = "class-signature-completer"
    }

    // MethodSigNamer
    case class MethodSigNamer(tree: Tree, rhs: Tree, tpt: Tree, tparams: List[TypeDef])
      extends TreeEvent with NamerEvent {
      override def tag = "method-signature-completer"
    }

    case class MissingParameterType(tree: Tree)
      extends TreeEvent with NamerEvent with HardErrorEvent {    
      override def tag = "missing-parameter-type"
      def errPos = tree.pos
    }

    // can't clone symbols as it causes cyclic references
    case class TypeDefSigNamer(tpsym: Symbol, rhs: Tree, tparams: List[TypeDef])
      extends TreeEvent with NamerEvent {
      override def tag = "abstract-type-signature-completer"
      def tree = rhs
    }

    case class ModuleSigNamer(templ: Template)
      extends TreeEvent with NamerEvent {
      def tree = templ
      override def tag = "object-signature-completer"
    }

    case class ValDefSigNamer(vdef: ValDef)
      extends TreeEvent with NamerEvent {
      def tree = vdef
      override def tag = super.tag + "-value-signature-completer"
    }
  }

  trait NamerExplanations {
    self: EventModel =>

    trait NamerExplanation

    case class MethodReturnType(tpe: Tree)
      extends Explanation with NamerExplanation

    case class MethodReturnTypeAgain(tpe: Tree)
      extends Explanation with NamerExplanation

    case class InferredMethodReturnType(rhs: Tree)
      extends Explanation with NamerExplanation

    case class ValExplicitType(tpe: Tree, sym: Symbol)
      extends Explanation with NamerExplanation

    case class TypeAbstractTpeBounds(bounds: Tree)
      extends Explanation with NamerExplanation

    case class TypeValDefBody(vdef: Tree, expectedPt: Boolean)
      extends Explanation with NamerExplanation

    case class TypeMethodDefBody(rhs: Tree)
      extends Explanation with NamerExplanation
  }
}