// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

opaque type OperationCall <: Node = Node

object OperationCall {
  extension (node: OperationCall) {
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

  def apply(node: Node): Either[String, OperationCall] =
    if node.tpe == "operation_call"
    then Right(node)
    else Left(s"Expected OperationCall, got ${node.tpe}")
  def unsafeApply(node: Node): OperationCall = apply(node).fold(sys.error, identity)
  def unapply(node: Node): Option[OperationCall] = apply(node).toOption
}

/*

*/
