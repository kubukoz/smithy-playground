// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node
import playground.treesitter4s.std.Selection

opaque type Whitespace <: Node = Node

object Whitespace {


  def apply(node: Node): Either[String, Whitespace] =
    if node.tpe == "whitespace"
    then Right(node)
    else Left(s"Expected Whitespace, got ${node.tpe}")

  def unsafeApply(node: Node): Whitespace = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[Whitespace] = apply(node).toOption

  final case class Selector(path: List[Whitespace]) extends Selection[Whitespace] {

  }
}
