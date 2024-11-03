import org.polyvariant.treesitter4s.Node
import org.polyvariant.treesitter4s.TreeSitterAPI
import playground.generated.nodes.*

val s =
  """
    use service foo.bar.baz.bax#Baz

    Bax { a = , x = 44 , y = 50}
    """.stripMargin

val p = TreeSitterAPI.make("smithyql")

val tree = p.parse(s)

SourceFile(tree.rootNode.get).use_clause.identifier.head.node.source

SourceFile(tree.rootNode.get).use_clause.identifier.selection.node.source

SourceFile(tree.rootNode.get)
  .use_clause
  .identifier
  .tail

val bind =
  SourceFile(tree.rootNode.get)
    .statements
    .operation_call
    .input
    .bindings
    .binding
    .find(_.key.source == "x")
    .get

bind.key.source

bind.value match {
  case InputNode.NumberCase(value) => value.source
}
