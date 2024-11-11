// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node
import playground.treesitter4s.std.Selection

opaque type Binding <: Node = Node

object Binding {
  extension (node: Binding) {
    def select[A](f: Binding.Selector => Selection[A]): List[A] = f(Binding.Selector(List(node))).path
    // fields
    def key: Option[Identifier] = node.fields.getOrElse("key", Nil).headOption.map {
      case Identifier(node) => node
    }

    def value: Option[InputNode] = node.fields.getOrElse("value", Nil).headOption.map {
      case InputNode(node) => node
    }
    // typed children

    // precise typed children

  }

  def apply(node: Node): Either[String, Binding] =
    if node.tpe == "binding"
    then Right(node)
    else Left(s"Expected Binding, got ${node.tpe}")

  def unsafeApply(node: Node): Binding = apply(node).fold(sys.error, identity)

  def unapply(node: Node): Option[Binding] = apply(node).toOption

  final case class Selector(path: List[Binding]) extends Selection[Binding] {
    def key: Identifier.Selector = Identifier.Selector(path.flatMap(_.key))
    def value: InputNode.Selector = InputNode.Selector(path.flatMap(_.value))
  }
}
