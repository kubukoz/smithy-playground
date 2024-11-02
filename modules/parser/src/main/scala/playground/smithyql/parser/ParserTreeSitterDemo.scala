package playground.smithyql

import org.polyvariant.treesitter4s.Node
import org.polyvariant.treesitter4s.TreeSitterAPI
import playground.generated.nodes.*

object ParserTreeSitterDemo extends App {

  val tree = TreeSitterAPI.make("smithyql").parse {
    """
    use service foo.bar.bax.qux#Baz

    Bax { x = 42 }
    """.stripMargin
  }

  val bind =
    Operation_call(Source_file(tree.rootNode.get).statements.children.head)
      .input
      .bindings
      .children
      .collect { case b @ Binding() => Binding(b) }
      .head

  println(bind.key.source + ": " + bind.value.node.source)
}
