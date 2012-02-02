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
    case class TypeSigNamer(tree0: Tree)
      extends TreeEvent with NamerEvent {
      val tree = duplicateTreeWithPos(tree0)
      override def tag = "type-signature-completer"
    }

    case class ClassSigNamer(templ0: Template, tparams0: List[TypeDef])
      extends TreeEvent with NamerEvent {
      val templ = duplicateTreeWithPos(templ0)
      val tparams = tparams0.map(duplicateTreeWithPos)
      def tree = templ
      override def tag = "class-signature-completer"
    }

    // MethodSigNamer
    case class MethodSigNamer(tree0: Tree, rhs0: Tree, tpt0: Tree, tparams0: List[TypeDef])
      extends TreeEvent with NamerEvent {
      val tree = duplicateTreeWithPos(tree0)
      val rhs = duplicateTreeWithPos(rhs0)
      val tpt = duplicateTreeWithPos(tpt0)
      val tparams = tparams0.map(duplicateTreeWithPos)
      override def tag = "method-signature-completer"
    }

    case class MissingParameterType(tree0: Tree)
      extends TreeEvent with NamerEvent with HardErrorEvent {
      val tree = duplicateTreeWithPos(tree0)      
      override def tag = "missing-parameter-type"
    }

    // can't clone symbols as it causes cyclic references
    case class TypeDefSigNamer(tpsym: Symbol, rhs0: Tree, tparams0: List[TypeDef])
      extends TreeEvent with NamerEvent {
      override def tag = "abstract-type-signature-completer"
      val rhs = duplicateTreeWithPos(rhs0)
      val tparams = tparams0.map(duplicateTreeWithPos)
      def tree = rhs
    }

    case class ModuleSigNamer(templ0: Template)
      extends TreeEvent with NamerEvent {
      val templ = duplicateTreeWithPos(templ0)
      def tree = templ
      override def tag = "object-signature-completer"
    }

    case class ValDefSigNamer(name: Name, rhs0: Tree, tpt0: Tree, vdef0: Tree)
      extends TreeEvent with NamerEvent {
      val rhs = duplicateTreeWithPos(rhs0)
      val tpt = duplicateTreeWithPos(tpt0)
      val vdef = duplicateTreeWithPos(vdef0)
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

    case class TypeValDefBody(rhs: Tree)
      extends Explanation with NamerExplanation

    case class TypeMethodDefBody(rhs: Tree)
      extends Explanation with NamerExplanation
  }
}