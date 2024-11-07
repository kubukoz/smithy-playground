// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

opaque type RunQuery <: Node = Node

object RunQuery {
  extension (node: RunQuery) {
    // fields
    def input: Option[Struct] = node.fields.getOrElse("input", Nil).headOption.map {
      case Struct(node) => node
    }

    def operation_name: Option[OperationName] = node.fields.getOrElse("operation_name", Nil).headOption.map {
      case OperationName(node) => node
    }
    // typed children

    // precise typed children

  }

  def apply(node: Node): Either[String, RunQuery] =
    if node.tpe == "run_query"
    then Right(node)
    else Left(s"Expected RunQuery, got ${node.tpe}")
  def unsafeApply(node: Node): RunQuery = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[RunQuery] = apply(node).toOption
}

/*

*/
