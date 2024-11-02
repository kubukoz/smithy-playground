// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

enum _Input_node {
  case BooleanCase(value: Boolean_)
  case ListCase(value: List_)
  case NullCase(value: Null_)
  case NumberCase(value: Number)
  case StringCase(value: String_)
  case StructCase(value: Struct)

  def asNode: Node = this match {
    case BooleanCase(value) => value.node
    case ListCase(value) => value.node
    case NullCase(value) => value.node
    case NumberCase(value) => value.node
    case StringCase(value) => value.node
    case StructCase(value) => value.node
  }
}

object _Input_node {
  def apply(node: Node): _Input_node = node match {
    case node @ Boolean_() => BooleanCase(Boolean_(node))
    case node @ List_() => ListCase(List_(node))
    case node @ Null_() => NullCase(Null_(node))
    case node @ Number() => NumberCase(Number(node))
    case node @ String_() => StringCase(String_(node))
    case node @ Struct() => StructCase(Struct(node))
  }
}

/*

*/
