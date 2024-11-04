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
  def apply(node: Node): Either[String, InputNode] = node match {
    case Boolean_(node) => Right(BooleanCase(node))
    case List_(node) => Right(ListCase(node))
    case Null_(node) => Right(NullCase(node))
    case Number(node) => Right(NumberCase(node))
    case String_(node) => Right(StringCase(node))
    case Struct(node) => Right(StructCase(node))
    case _ => Left(s"Expected InputNode, got ${node.tpe}")
  }

  def unapply(node: Node): Option[InputNode] = apply(node).toOption
}
/*

*/
