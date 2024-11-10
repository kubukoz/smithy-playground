// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

opaque type Boolean_ <: Node = Node

object Boolean_ {


  def apply(node: Node): Either[String, Boolean_] =
    if node.tpe == "boolean"
    then Right(node)
    else Left(s"Expected Boolean_, got ${node.tpe}")
  def unsafeApply(node: Node): Boolean_ = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[Boolean_] = apply(node).toOption
}
