// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

opaque type List_ <: Node = Node

object List_ {
  extension (node: List_) {
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
}
