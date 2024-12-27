// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node
import playground.treesitter4s.std.Selection

opaque type OperationName <: Node = Node

object OperationName {
  extension (node: OperationName) {
    def select[A](f: OperationName.Selector => Selection[A]): List[A] = f(OperationName.Selector(List(node))).path
    // fields

    // typed children
    def typedChildren: Option[Identifier] = node.children.collectFirst {
      case Identifier(node) => node
    }
    // precise typed children
    def identifier: Option[Identifier] = node.children.collectFirst {
      case Identifier(node) => node
    }
  }

  def apply(node: Node): Either[String, OperationName] =
    if node.tpe == "operation_name"
    then Right(node)
    else Left(s"Expected OperationName, got ${node.tpe}")

  def unsafeApply(node: Node): OperationName = apply(node).fold(sys.error, identity)

  def unapply(node: Node): Option[OperationName] = apply(node).toOption

  final case class Selector(path: List[OperationName]) extends Selection[OperationName] {
    def identifier: Identifier.Selector = Identifier.Selector(path.flatMap(_.identifier))

    type Self = Selector
    protected val remake = Selector.apply
  }
}
