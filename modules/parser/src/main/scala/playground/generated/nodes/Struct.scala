// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

opaque type Struct <: Node = Node

object Struct {
  extension (node: Struct) {
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
}

/*

*/
