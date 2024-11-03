// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class Binding /* private */(node: Node) extends Node {
  // fields
  def key: Identifier = node.fields("key").head match {
    case node @ Identifier() => Identifier(node)
  }

  def value: InputNode = node.fields("value").head match {
    case node @ InputNode() => InputNode(node)
  }
  // typed children

  // precise typed children


  export node.*
}

object Binding {
  def unapply(node: Node): Boolean = node.tpe == "binding"
}

/*

*/
