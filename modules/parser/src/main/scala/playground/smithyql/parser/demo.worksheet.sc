import org.polyvariant.treesitter4s.Node
import org.polyvariant.treesitter4s.TreeSitterAPI
import playground.generated.nodes.*

val s =
  """
use service foo.bar.baz.bax#Baz

Bax { a = , x = 44  y = 50, b = ,,42, xyz = { a = }}
    """.stripMargin

val p = TreeSitterAPI.make("smithyql")

// hmm
// https://github.com/Jakobeha/type-sitter/blob/5d2cff2f2641d2af9a9a7ebdcd49b0311f19ad66/type-sitter-lib/src/node/incorrect_kind.rs#L10
/// Underlying cause of why the node is the wrong kind
// pub enum IncorrectKindCause {
//     /// Node is an error node
//     Error,
//     /// Node is a missing node
//     Missing,
//     /// Node is valid but simply of a different kind (bad node-types.json? Different language?
//     /// Broken user invariant?)
//     OtherKind(&'static str),
// }
case class ErrorNode(node: Node) extends Node {
  export node.*
}

val base = p.parse("Foo { true }").rootNode.get
SourceFile
  .unsafeApply(base)
  .statements
  .get
  .operation_call
  .get
  .input
  .get
  .children
  .find(_.isError)

// SourceFile(p.parse("use service foo.bar.baz.bax#Baz").rootNode.get).statements
// SourceFile(p.parse("use service foo.bar.baz.bax#Baz").rootNode.get).isError
// SourceFile(p.parse("use service foo.bar.baz.bax#Baz").rootNode.get).hasError
// SourceFile(p.parse("use service foo.bar.baz.bax#Baz").rootNode.get).children.map(_.tpe)

val tree = p.parse(s)

tree.rootNode.get.text

SourceFile.unsafeApply(tree.rootNode.get).use_clause.get.identifier.head.source

SourceFile.unsafeApply(tree.rootNode.get).use_clause.get.identifier.get.selection.get.source

SourceFile
  .unsafeApply(tree.rootNode.get)
  .use_clause
  .get
  .identifier
  .tail

SourceFile
  .unsafeApply(tree.rootNode.get)
  .statements
  .get
  .operation_call
  .get
  .input
  .get
  .children
  .filter(_.tpe == "ERROR")
  .head
  .source

tree
  .rootNode
  .get
  .fold[List[Node]]((n, ch) => n :: ch.toList.flatten)
  .filter(_.tpe == "ERROR")
  .map(_.source)

tree
  .rootNode
  .get
  .fold[List[Node]]((n, ch) => n :: ch.toList.flatten)
  .filter(_.isMissing)

tree
  .rootNode
  .get
  .fold[List[Node]]((n, ch) => n :: ch.toList.flatten)
  .filter(_.isExtra)
  .size

tree
  .rootNode
  .get
  .fold[List[Node]]((n, ch) => n :: ch.toList.flatten)
  .filter(_.isError)
  .map(_.text)
  .mkString("\n")

tree
  .rootNode
  .get
  .fold[List[Node]]((n, ch) => n :: ch.toList.flatten)
  .size

tree
  .rootNode
  .get
  .fold[List[Node]]((n, ch) => n :: ch.toList.flatten)
  .filter(_.hasError)
  .size

tree
  .rootNode
  .get
  .fold[List[Node]]((n, ch) => n :: ch.toList.flatten)
  // .filter(_.tpe == "MISSING")
  // .map(_.source)
  .map(_.tpe)
  .toSet

val bind =
  SourceFile
    .unsafeApply(tree.rootNode.get)
    .statements
    .get
    .operation_call
    .get
    .input
    .get
    .bindings
    .get
    .binding

// .find(_.key.source == "x")
// .get

bind.size
bind.head.fields
bind.map(_.key.get.source)

bind.last.value.get.asStruct.get.bindings.get.binding.head.value.get.asBoolean.get.tpe
// bind.key.source

// bind.value.asNumber.get.source
