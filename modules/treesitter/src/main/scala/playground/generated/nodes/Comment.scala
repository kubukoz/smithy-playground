// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

opaque type Comment <: Node = Node

object Comment {


  def apply(node: Node): Either[String, Comment] =
    if node.tpe == "comment"
    then Right(node)
    else Left(s"Expected Comment, got ${node.tpe}")
  def unsafeApply(node: Node): Comment = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[Comment] = apply(node).toOption
}
