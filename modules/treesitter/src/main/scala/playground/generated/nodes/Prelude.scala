// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

opaque type Prelude <: Node = Node

object Prelude {
  extension (node: Prelude) {
    // fields

    // typed children
    def typedChildren: List[UseClause] = node.children.toList.collect {
      case UseClause(node) => node
    }
    // precise typed children
    def use_clause: List[UseClause] = node.children.toList.collect {
      case UseClause(node) => node
    }
  }

  def apply(node: Node): Either[String, Prelude] =
    if node.tpe == "prelude"
    then Right(node)
    else Left(s"Expected Prelude, got ${node.tpe}")
  def unsafeApply(node: Node): Prelude = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[Prelude] = apply(node).toOption
}
