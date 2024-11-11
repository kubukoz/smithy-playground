// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node
import playground.treesitter4s.std.Selection

opaque type List_ <: Node = Node

object List_ {
  extension (node: List_) {
    def select[A](f: List_.Selector => Selection[A]): List[A] = f(List_.Selector(List(node))).path
    // fields
    def list_fields: List[InputNode] = node.fields.getOrElse("list_fields", Nil).toList.collect {
      case InputNode(node) => node
    }
    // typed children

    // precise typed children

  }

  def apply(node: Node): Either[String, List_] =
    if node.tpe == "list"
    then Right(node)
    else Left(s"Expected List_, got ${node.tpe}")

  def unsafeApply(node: Node): List_ = apply(node).fold(sys.error, identity)

  def unapply(node: Node): Option[List_] = apply(node).toOption

  final case class Selector(path: List[List_]) extends Selection[List_] {
    def list_fields: InputNode.Selector = InputNode.Selector(path.flatMap(_.list_fields))
  }
}
