// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node

case class Struct /* private */(node: Node) extends Node {
  def bindings: Bindings = node.fields("bindings").head match {
    case node @ Bindings() => Bindings(node)
  }


  export node.*
}

object Struct {
  def unapply(node: Node): Boolean = node.tpe == "struct"
}

/*

*/
