package scala.reflect
package internal
package event

trait NamerEventsUniverse {
  out: SymbolTable with EventsUniverse =>

  trait NamerEvents {
    self: EventModel =>

    trait NamerEvent {
      def tag = "namer"
    }

    case class NamerDone(originEvent: Int) extends Event with NamerEvent with DoneBlock {
      override def tag = super.tag + "-done"
      def participants: List[Any] = List()
      override def eventString = ""
    }

    // TODO: get rid off that one?
    case class TypeSigNamer(tree: Tree)
      extends TreeEvent with NamerEvent {
      //override def tag = super.tag + "-type-signature-completer"
      override def tag = "type-signature-completer"
      override def lName = "Typing of\n type's signature"
      override protected def eventStringRep = "Tree to be typed: " + tree
    }

    case class ClassSigNamer(templ: Template, tparams: List[TypeDef])
      extends TreeEvent with NamerEvent {
      //override def tag = super.tag + "-class-signature-completer"
      override def tag = "class-signature-completer"
      override def lName = "Typing of\n class' signature"
      def tree = templ
      override protected def eventStringRep =
        "Completing class " + templ + " with type params: " + tparams
    }

    // MethodSigNamer

    case class MethodSigNamer(tree: Tree, rhs: Tree, tpt: Tree, tparams: List[TypeDef])
      extends TreeEvent with NamerEvent {
      //override def tag = super.tag + "-method-signature-completer"
      override def tag = "method-signature-completer"
      override def lName = "Typing of\n method's signature"
      override protected def eventStringRep =
        "Completing method of type " + tpt + " with type params: " + tparams +
         "\n" + rhs
    }

    case class MissingParameterType(tree: Tree)
      extends TreeEvent with NamerEvent with HardErrorEvent {
      override def tag = "missing-parameter-type"
      override def lName = "Missing parameter type"
      override protected def eventStringRep =
        "Missing parameter type for " + tree
    }

    case class TypeDefSigNamer(tpsym: Symbol, rhs: Tree, tparams: List[TypeDef])
      extends TreeEvent with NamerEvent {
      //override def tag = super.tag + "-abstract-type-signature-completer"
      override def tag = "abstract-type-signature-completer"
      override def lName = "Typing of\n abstract type's signature"
      def tree = rhs
      override protected def eventStringRep =
        "Completing abstract type definition " + tpsym + " with type params: " + tparams +
        "\n" + rhs
    }

    case class ModuleSigNamer(templ: Template)
      extends TreeEvent with NamerEvent {
      //override def tag = super.tag + "-module-signature-completer"
      override def tag = "object-signature-completer"
      override def lName = "Typing \n object's signature"
      def tree = templ
      override protected def eventStringRep =
        "Completing object " + templ
    }

    case class ValDefSigNamer(name: Name, rhs: Tree, tpt: Tree, vdef: Tree)
      extends TreeEvent with NamerEvent {
      override def tag = super.tag + "-value-signature-completer"
      //override def lName = "value-signature-completer"
      override def lName = "Typing of\n value's signature"
      val tree = duplicateTreeWithPos(vdef)
      override protected def eventStringRep =
        "Completing value definition " + name + ": " + tpt + "\n" +
        (if (tpt.isEmpty) "Compute type from the body of the value " else "Type type" )+
        "\n" + rhs
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