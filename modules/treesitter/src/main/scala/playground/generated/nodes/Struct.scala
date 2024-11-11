// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node
import playground.treesitter4s.std.Selection

opaque type Struct <: Node = Node

object Struct {
  extension (node: Struct) {
    def select[A](f: Struct.Selector => Selection[A]): List[A] = f(Struct.Selector(List(node))).path
    // fields
    def bindings: List[Binding] = node.fields.getOrElse("bindings", Nil).toList.collect {
      case Binding(node) => node
    }
    // typed children

    // precise typed children

  }

  def apply(node: Node): Either[String, Struct] =
    if node.tpe == "struct"
    then Right(node)
    else Left(s"Expected Struct, got ${node.tpe}")

  def unsafeApply(node: Node): Struct = apply(node).fold(sys.error, identity)

  def unapply(node: Node): Option[Struct] = apply(node).toOption

  final case class Selector(path: List[Struct]) extends Selection[Struct] {
    def bindings: Binding.Selector = Binding.Selector(path.flatMap(_.bindings))
  }
}
