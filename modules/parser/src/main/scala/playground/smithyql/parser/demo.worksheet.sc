import org.polyvariant.treesitter4s.Node
import org.polyvariant.treesitter4s.TreeSitterAPI
import playground.generated.nodes.*

val s =
  """
    use service foo.bar.baz.bax#Baz

    Bax { x = 42 , y = 50}
    """.stripMargin

val p = TreeSitterAPI.make("smithyql")

val tree = p.parse(s)

Source_file(tree.rootNode.get).use_clause.identifier.head.node.source
//
Source_file(tree.rootNode.get).use_clause.identifier.selection.node.source

Source_file(tree.rootNode.get)
  .use_clause
  .identifier
  .tail

val bind =
  Operation_call(Source_file(tree.rootNode.get).statements.children.head)
    .input
    .bindings
    .children
    .collect { case b @ Binding() => Binding(b) }
    .head

bind.key.source

bind.value
