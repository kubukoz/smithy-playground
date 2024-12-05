// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node
import playground.treesitter4s.std.Selection

opaque type Prelude <: Node = Node

object Prelude {
  extension (node: Prelude) {
    def select[A](f: Prelude.Selector => Selection[A]): List[A] = f(Prelude.Selector(List(node))).path
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

  final case class Selector(path: List[Prelude]) extends Selection[Prelude] {
    def use_clause: UseClause.Selector = UseClause.Selector(path.flatMap(_.use_clause))

    type Self = Selector
    protected val remake = Selector.apply
  }
}
