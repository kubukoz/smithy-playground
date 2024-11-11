// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node
import playground.treesitter4s.std.Selection

opaque type TopLevelStatement <: Node = Node

object TopLevelStatement {
  extension (node: TopLevelStatement) {
    def select[A](f: TopLevelStatement.Selector => Selection[A]): List[A] = f(TopLevelStatement.Selector(List(node))).path
    // fields

    // typed children
    def typedChildren: Option[RunQuery] = node.children.collectFirst {
      case RunQuery(node) => node
    }
    // precise typed children
    def run_query: Option[RunQuery] = node.children.collectFirst {
      case RunQuery(node) => node
    }
  }

  def apply(node: Node): Either[String, TopLevelStatement] =
    if node.tpe == "top_level_statement"
    then Right(node)
    else Left(s"Expected TopLevelStatement, got ${node.tpe}")

  def unsafeApply(node: Node): TopLevelStatement = apply(node).fold(sys.error, identity)

  def unapply(node: Node): Option[TopLevelStatement] = apply(node).toOption

  final case class Selector(path: List[TopLevelStatement]) extends Selection[TopLevelStatement] {
    def run_query: RunQuery.Selector = RunQuery.Selector(path.flatMap(_.run_query))
  }
}
