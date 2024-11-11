// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node
import playground.treesitter4s.std.Selection

opaque type Identifier <: Node = Node

object Identifier {


  def apply(node: Node): Either[String, Identifier] =
    if node.tpe == "identifier"
    then Right(node)
    else Left(s"Expected Identifier, got ${node.tpe}")

  def unsafeApply(node: Node): Identifier = apply(node).fold(sys.error, identity)

  def unapply(node: Node): Option[Identifier] = apply(node).toOption

  final case class Selector(path: List[Identifier]) extends Selection[Identifier] {

  }
}
