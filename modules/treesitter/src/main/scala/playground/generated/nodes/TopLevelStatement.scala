// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

opaque type TopLevelStatement <: Node = Node

object TopLevelStatement {
  extension (node: TopLevelStatement) {
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
}
