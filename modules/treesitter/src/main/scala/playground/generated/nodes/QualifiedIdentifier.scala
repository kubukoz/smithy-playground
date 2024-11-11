// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node
import playground.treesitter4s.std.Selection

opaque type QualifiedIdentifier <: Node = Node

object QualifiedIdentifier {
  extension (node: QualifiedIdentifier) {
    def select[A](f: QualifiedIdentifier.Selector => Selection[A]): List[A] = f(QualifiedIdentifier.Selector(List(node))).path
    // fields
    def namespace: List[Identifier] = node.fields.getOrElse("namespace", Nil).toList.collect {
      case Identifier(node) => node
    }

    def selection: Option[Identifier] = node.fields.getOrElse("selection", Nil).headOption.map {
      case Identifier(node) => node
    }
    // typed children

    // precise typed children

  }

  def apply(node: Node): Either[String, QualifiedIdentifier] =
    if node.tpe == "qualified_identifier"
    then Right(node)
    else Left(s"Expected QualifiedIdentifier, got ${node.tpe}")

  def unsafeApply(node: Node): QualifiedIdentifier = apply(node).fold(sys.error, identity)

  def unapply(node: Node): Option[QualifiedIdentifier] = apply(node).toOption

  final case class Selector(path: List[QualifiedIdentifier]) extends Selection[QualifiedIdentifier] {
    def namespace: Identifier.Selector = Identifier.Selector(path.flatMap(_.namespace))
    def selection: Identifier.Selector = Identifier.Selector(path.flatMap(_.selection))

    type Self = Selector
    protected val remake = Selector.apply
  }
}
