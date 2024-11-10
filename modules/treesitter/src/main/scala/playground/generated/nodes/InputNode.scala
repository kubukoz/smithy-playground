// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

opaque type InputNode <: Node = Boolean_ | List_ | Null_ | Number | String_ | Struct

object InputNode {

  extension (node: InputNode) {
    def asBoolean: Option[Boolean_] = Boolean_.unapply(node)
    def asList: Option[List_] = List_.unapply(node)
    def asNull: Option[Null_] = Null_.unapply(node)
    def asNumber: Option[Number] = Number.unapply(node)
    def asString: Option[String_] = String_.unapply(node)
    def asStruct: Option[Struct] = Struct.unapply(node)
  }

  def apply(node: Node): Either[String, InputNode] = node match {
    case Boolean_(node) => Right(node)
    case List_(node) => Right(node)
    case Null_(node) => Right(node)
    case Number(node) => Right(node)
    case String_(node) => Right(node)
    case Struct(node) => Right(node)
    case _ => Left(s"Expected InputNode, got ${node.tpe}")
  }

  def unapply(node: Node): Option[InputNode] = apply(node).toOption
}
