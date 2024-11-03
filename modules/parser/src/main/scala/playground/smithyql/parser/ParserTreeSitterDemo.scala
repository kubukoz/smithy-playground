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
    SourceFile(tree.rootNode.get)
      .statements
      .operation_call
      .input
      .bindings
      .binding
      .find(_.key.source == "x")
      .get

  bind.value match {
    case InputNode.NumberCase(value) => println(value.source)
  }
}
