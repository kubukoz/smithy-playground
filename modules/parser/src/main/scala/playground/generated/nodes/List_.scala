// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class List_ /* private */(node: Node) extends Node {

  def list_fields: List_fields = List_fields(node.fields("list_fields").head)

  export node.*
}

object List_ {
  def unapply(node: Node): scala.Boolean = node.tpe == "list"
}

/*

*/
