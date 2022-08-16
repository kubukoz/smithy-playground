package playground.smithyql

import ai.serenade.treesitter.Languages
import ai.serenade.treesitter.Node
import ai.serenade.treesitter.Parser
import cats.implicits._

import java.io.Closeable
import scala.util.Using
import cats.data.NonEmptyList
import cats.Id
import cats.Monad

object ParserAdapter {

  System.load("/Users/kubukoz/projects/java-tree-sitter/out.dylib")

  implicit object CloseableIsReleasable extends Using.Releasable[Closeable] {
    def release(resource: Closeable): Unit = resource.close()
  }

  def children(node: Node): List[Node] = List.tabulate(node.getChildCount())(node.getChild(_))

  def downType(
    tpe: String
  )(
    node: Node
  ): Node = downTypeOpt(tpe)(node)
    .getOrElse(
      sys.error(
        "no children with type " + tpe + ", existing types: " + children(node)
          .map(_.getType())
          .mkString(",")
      )
    )

  def downTypeAll(
    tpe: String
  )(
    node: Node
  ): List[Node] = children(node).filter(_.getType() == tpe)

  def downTypeOpt(
    tpe: String
  )(
    node: Node
  ): Option[Node] = children(node)
    .find(_.getType() == tpe)

  def childTypes(
    node: Node
  ): String = children(node)
    .map(_.getType())
    .mkString(", ")

  object ast {

    case class QualifiedIdentifier[F[_]](
      path: F[NonEmptyList[Identifier[F]]],
      selection: F[Identifier[F]],
    ) {

      def sequenceK(
        implicit F: Monad[F]
      ): F[QualifiedIdentifier[Id]] =
        (
          path.flatMap(_.nonEmptyTraverse(_.sequenceK)),
          selection.flatMap(_.sequenceK),
        )
          .mapN(QualifiedIdentifier.apply[Id])

    }

    case class UseClause[F[_]](
      ident: F[QualifiedIdentifier[F]]
    )

    case class Op[F[_]](
      useClause: F[Option[UseClause[F]]],
      name: F[String],
      input: F[Struct[F]],
    )

    case class Identifier[F[_]](value: F[String]) {
      def sequenceK(implicit F: Monad[F]): F[Identifier[Id]] = value.map(Identifier.apply[Id])
    }

    case class Field[F[_]](identifier: F[Identifier[F]], inputNode: F[InputNode[F]])
    case class Fields[F[_]](fields: F[List[Field[F]]])
    case class Struct[F[_]](fields: F[Option[Fields[F]]])
    case class ListItems[F[_]](items: F[List[InputNode[F]]])
    case class Listed[F[_]](items: F[Option[ListItems[F]]])
    sealed trait InputNode[F[_]]

    object InputNode {
      case class StructCase[F[_]](s: Struct[F]) extends InputNode[F]
      case class ListCase[F[_]](l: Listed[F]) extends InputNode[F]
      case class NumberCase[F[_]](n: F[String]) extends InputNode[F]
      case class StringCase[F[_]](n: F[String]) extends InputNode[F]
      case class BoolCase[F[_]](v: F[Boolean]) extends InputNode[F]
      case class NullCase[F[_]]() extends InputNode[F]
    }

  }

  object ast1 {

    type QualifiedIdentifier = ast.QualifiedIdentifier[Id]
    val QualifiedIdentifier = ast.QualifiedIdentifier[Id]
    type UseClause = ast.UseClause[Id]
    val UseClause = ast.UseClause[Id]
    type Op = ast.Op[Id]
    val Op = ast.Op[Id]
    type Identifier = ast.Identifier[Id]
    val Identifier = ast.Identifier[Id]
    type Field = ast.Field[Id]
    val Field = ast.Field[Id]
    type Fields = ast.Fields[Id]
    val Fields = ast.Fields[Id]
    type Struct = ast.Struct[Id]
    val Struct = ast.Struct[Id]
    type ListItems = ast.ListItems[Id]
    val ListItems = ast.ListItems[Id]
    type Listed = ast.Listed[Id]
    val Listed = ast.Listed[Id]
    type InputNode = ast.InputNode[Id]

    object InputNode {
      type StructCase = ast.InputNode.StructCase[Id]
      val StructCase = ast.InputNode.StructCase[Id]
      type ListCase = ast.InputNode.ListCase[Id]
      val ListCase = ast.InputNode.ListCase[Id]
      type NumberCase = ast.InputNode.NumberCase[Id]
      val NumberCase = ast.InputNode.NumberCase[Id]
      type StringCase = ast.InputNode.StringCase[Id]
      val StringCase = ast.InputNode.StringCase[Id]
      type BoolCase = ast.InputNode.BoolCase[Id]
      val BoolCase = ast.InputNode.BoolCase[Id]
      type NullCase = ast.InputNode.NullCase[Id]
      val NullCase = ast.InputNode.NullCase[Id]()
    }

  }

  def union[A](decoders: (String, Node => A)*): Node => A =
    node =>
      decoders
        .collectFirstSome { case (key, v) => downTypeOpt(key)(node).map(v) }
        .getOrElse(sys.error("missing case: " + childTypes(node)))

  import ast1._

  class OpDecoders(src: String) {
    def text(node: Node) = src.substring(node.getStartByte(), node.getEndByte())

    val identifier: Node => Identifier = text.map(Identifier(_))

    val listItems: Node => ListItems = downTypeAll("input_node")
      .map(_.map(inputNode))
      .map(ListItems.apply)

    val listed: Node => Listed = downTypeOpt("list_fields")
      .map(_.map(listItems))
      .map(Listed.apply)

    val bool: Node => Boolean = text
      .map(_.toBoolean)

    lazy val inputNode: Node => InputNode = union(
      "number" -> text.map(InputNode.NumberCase(_)),
      "string" -> text.map(InputNode.StringCase(_)),
      "null" -> Function.const(InputNode.NullCase),
      "struct" -> struct.map(InputNode.StructCase(_)),
      "list" -> listed.map(InputNode.ListCase(_)),
      "boolean" -> bool.map(InputNode.BoolCase(_)),
    )

    val field: Node => Field =
      (
        downType("identifier") >>> identifier,
        downType("input_node") >>> inputNode,
      ).mapN(Field.apply)

    val fields: Node => Fields = downTypeAll("field")
      .map(_.map(field))
      .map(Fields.apply)

    lazy val struct: Node => Struct = downTypeOpt("fields")
      .map(_.map(fields))
      .map(Struct.apply)

    val qualifiedIdentifier: Node => QualifiedIdentifier = downTypeAll("identifier")
      .map {
        case _ :: Nil | Nil => sys.error("no ident??")
        case all =>
          val prefix = NonEmptyList.fromListUnsafe(all.init)

          QualifiedIdentifier(prefix.map(identifier), identifier(all.last))
      }

    val useClause: Node => UseClause =
      (
        downType("qualified_identifier") >>>
          qualifiedIdentifier
      )
        .map(UseClause)

    val op: Node => Op =
      (
        downTypeOpt("use_clause").map(_.map(useClause)),
        downType("operation_name") >>>
          downType("identifier") >>>
          text,
        downType("struct") >>>
          struct,
      ).mapN(Op.apply)

  }

  def parse(s: String) = {

    val p = new Parser()
    p.setLanguage(Languages.smithyql())
    Using.resource(p.parseString(s)) { tree =>
      new OpDecoders(s).op(tree.getRootNode())
    }
  }

}

object ParserAdapterDemo extends App {
  println(
    ParserAdapter
      .parse(
        "use service a.b#C\n helalsdfhl //a\n{ hello = 42, foo = 50, x = { y = \"hello\"} , z = null, aa = [10, true, false, null]}"
      )
  )
}
