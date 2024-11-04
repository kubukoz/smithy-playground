// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

opaque type Bindings <: Node = Node

object Bindings {
  extension (node: Bindings) {
    // fields

    // typed children
    def typedChildren: List[Binding] = node.children.toList.collect {
      case Binding(node) => node
    }
    // precise typed children
    def binding: List[Binding] = node.children.toList.collect {
      case Binding(node) => node
    }
  }

  def apply(node: Node): Either[String, Bindings] =
    if node.tpe == "bindings"
    then Right(node)
    else Left(s"Expected Bindings, got ${node.tpe}")
  def unsafeApply(node: Node): Bindings = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[Bindings] = apply(node).toOption
}

/*

*/
