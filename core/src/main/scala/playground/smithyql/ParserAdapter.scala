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
import cats.Applicative
import scala.util.matching.Regex
import cats.~>
import cats.StackSafeMonad

object ParserAdapter {

  System.load("/Users/kubukoz/projects/java-tree-sitter/out.dylib")

  implicit object CloseableIsReleasable extends Using.Releasable[Closeable] {
    def release(resource: Closeable): Unit = resource.close()
  }

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

  import ast1._

  trait NodeDecoder[+A] {
    def decode(node: Node): A
    def >>>[B](another: NodeDecoder[B])(implicit aIsNode: A <:< Node): NodeDecoder[B] =
      node => another.decode(decode(node))
  }

  object NodeDecoder {

    implicit val instances: Monad[NodeDecoder] =
      new StackSafeMonad[NodeDecoder] {

        def flatMap[A, B](fa: NodeDecoder[A])(f: A => NodeDecoder[B]): NodeDecoder[B] = { node =>
          f(fa.decode(node)).decode(node)
        }

        def pure[A](x: A): NodeDecoder[A] = _ => x

      }

    val children: NodeDecoder[List[Node]] =
      node => List.tabulate(node.getChildCount())(node.getChild(_))

    def downType(
      tpe: String
    ): NodeDecoder[Node] =
      node =>
        downTypeOpt(tpe)
          .decode(node)
          .getOrElse(
            sys.error(
              "no children with type " + tpe + ", existing types: " + children
                .decode(node)
                .map(_.getType())
                .mkString(",")
            )
          )

    def downTypeAll(
      tpe: String
    ): NodeDecoder[List[Node]] = children.map(_.filter(_.getType() == tpe))

    def downTypeOpt(
      tpe: String
    ): NodeDecoder[Option[Node]] = children
      .map(_.find(_.getType() == tpe))

    def childTypes: NodeDecoder[String] = children
      .map(_.map(_.getType()).mkString(", "))

    def union[A](decoders: (String, NodeDecoder[A])*): NodeDecoder[A] =
      node =>
        decoders
          .collectFirstSome { case (key, v) => downTypeOpt(key).decode(node).map(v.decode) }
          .getOrElse(sys.error("missing case: " + childTypes.decode(node)))

  }

  class OpDecoders(src: String) {
    import NodeDecoder._

    val text: NodeDecoder[String] = node => src.substring(node.getStartByte(), node.getEndByte())

    val identifier: NodeDecoder[Identifier] = text.map(Identifier(_))

    val listItems: NodeDecoder[ListItems] = downTypeAll("input_node")
      .map(_.map(inputNode.decode))
      .map(ListItems.apply)

    val listed: NodeDecoder[Listed] = downTypeOpt("list_fields")
      .map(_.map(listItems.decode))
      .map(Listed.apply)

    val bool: NodeDecoder[Boolean] = text
      .map(_.toBoolean)

    lazy val inputNode: NodeDecoder[InputNode] = union(
      "number" -> text.map(InputNode.NumberCase(_)),
      "string" -> text.map(InputNode.StringCase(_)),
      "null" -> InputNode.NullCase.pure[NodeDecoder],
      "struct" -> struct.map(InputNode.StructCase(_)),
      "list" -> listed.map(InputNode.ListCase(_)),
      "boolean" -> bool.map(InputNode.BoolCase(_)),
    )

    val field: NodeDecoder[Field] =
      (
        downType("identifier") >>> identifier,
        downType("input_node") >>> inputNode,
      ).mapN(Field.apply)

    val fields: NodeDecoder[Fields] = downTypeAll("field")
      .map(_.map(field.decode))
      .map(Fields.apply)

    lazy val struct: NodeDecoder[Struct] = downTypeOpt("fields")
      .map(_.map(fields.decode))
      .map(Struct.apply)

    val qualifiedIdentifier: NodeDecoder[QualifiedIdentifier] = downTypeAll("identifier")
      .map {
        case _ :: Nil | Nil => sys.error("no ident??")
        case all =>
          val prefix = NonEmptyList.fromListUnsafe(all.init)

          QualifiedIdentifier(prefix.map(identifier.decode), identifier.decode(all.last))
      }

    val useClause: NodeDecoder[UseClause] =
      (
        downType("qualified_identifier") >>>
          qualifiedIdentifier
      )
        .map(UseClause)

    val op: NodeDecoder[Op] =
      (
        downTypeOpt("use_clause").map(_.map(useClause.decode)),
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
      new OpDecoders(s).op.decode(tree.getRootNode())
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
