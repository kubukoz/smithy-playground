// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

opaque type Number <: Node = Node

object Number {


  def apply(node: Node): Either[String, Number] =
    if node.tpe == "number"
    then Right(node)
    else Left(s"Expected Number, got ${node.tpe}")
  def unsafeApply(node: Node): Number = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[Number] = apply(node).toOption
}
