// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

enum InputNode {
  private case BooleanCase(value: Boolean_)
  private case ListCase(value: List_)
  private case NullCase(value: Null_)
  private case NumberCase(value: Number)
  private case StringCase(value: String_)
  private case StructCase(value: Struct)

  def asBoolean: Option[Boolean_] = this match { case BooleanCase(v) => Some(v); case _ => None }
  def asList: Option[List_] = this match { case ListCase(v) => Some(v); case _ => None }
  def asNull: Option[Null_] = this match { case NullCase(v) => Some(v); case _ => None }
  def asNumber: Option[Number] = this match { case NumberCase(v) => Some(v); case _ => None }
  def asString: Option[String_] = this match { case StringCase(v) => Some(v); case _ => None }
  def asStruct: Option[Struct] = this match { case StructCase(v) => Some(v); case _ => None }

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
