// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

opaque type TopLevelStatement <: Node = Node

object TopLevelStatement {
  extension (node: TopLevelStatement) {
    // fields

    // typed children
    def typedChildren: Option[LetBinding | OperationCall] = node.children.collectFirst {
      case LetBinding(node) => node
      case OperationCall(node) => node
    }
    // precise typed children
    def let_binding: Option[LetBinding] = node.children.collectFirst {
      case LetBinding(node) => node
    }

    def operation_call: Option[OperationCall] = node.children.collectFirst {
      case OperationCall(node) => node
    }
  }

  def apply(node: Node): Either[String, TopLevelStatement] =
    if node.tpe == "top_level_statement"
    then Right(node)
    else Left(s"Expected TopLevelStatement, got ${node.tpe}")
  def unsafeApply(node: Node): TopLevelStatement = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[TopLevelStatement] = apply(node).toOption
}

/*

*/
