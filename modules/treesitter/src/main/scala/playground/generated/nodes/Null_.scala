// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node
import playground.treesitter4s.std.Selection

opaque type Null_ <: Node = Node

object Null_ {


  def apply(node: Node): Either[String, Null_] =
    if node.tpe == "null"
    then Right(node)
    else Left(s"Expected Null_, got ${node.tpe}")

  def unsafeApply(node: Node): Null_ = apply(node).fold(sys.error, identity)

  def unapply(node: Node): Option[Null_] = apply(node).toOption

  final case class Selector(path: List[Null_]) extends Selection[Null_] {


    type Self = Selector
    protected val remake = Selector.apply
  }
}
