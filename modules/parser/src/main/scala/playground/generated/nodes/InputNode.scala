// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

enum InputNode {
  case BooleanCase(value: Boolean_)
  case ListCase(value: List_)
  case NullCase(value: Null_)
  case NumberCase(value: Number)
  case StringCase(value: String_)
  case StructCase(value: Struct)

  def node: Node = this match {
    case BooleanCase(value) => value.node
    case ListCase(value) => value.node
    case NullCase(value) => value.node
    case NumberCase(value) => value.node
    case StringCase(value) => value.node
    case StructCase(value) => value.node
  }
}

object InputNode {
  def apply(node: Node): InputNode = node match {
    case node @ Boolean_() => BooleanCase(Boolean_(node))
    case node @ List_() => ListCase(List_(node))
    case node @ Null_() => NullCase(Null_(node))
    case node @ Number() => NumberCase(Number(node))
    case node @ String_() => StringCase(String_(node))
    case node @ Struct() => StructCase(Struct(node))
  }

  def unapply(node: Node): Boolean = node match {
    case node @ Boolean_() => true
    case node @ List_() => true
    case node @ Null_() => true
    case node @ Number() => true
    case node @ String_() => true
    case node @ Struct() => true
  }
}

/*

*/
