// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class Struct /* private */(node: Node) extends Node {

  def bindings: Bindings = Bindings(node.fields("bindings").head)

  export node.*
}

object Struct {
  def unapply(node: Node): scala.Boolean = node.tpe == "struct"
}

/*

*/
