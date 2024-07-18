package playground.smithyql.parser.v3

import cats.Applicative
import cats.Id
import cats.Monad
import cats.Parallel
import cats.StackSafeMonad
import cats.arrow.FunctionK
import cats.data.EitherNel
import cats.data.NonEmptyList
import cats.implicits._
import org.antlr.v4.runtime.BaseErrorListener
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.Parser
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.RecognitionException
import org.antlr.v4.runtime.Recognizer
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.parser.v3.Demo.Schema.Mapped
import playground.smithyql.parser.v3.Demo.Schema.Node
import playground.smithyql.parser.v3.Demo.Schema.Repeat
import playground.smithyql.parser.v3.Demo.Schema.Repeat1
import playground.smithyql.parser.v3.Demo.Schema.Struct
import playground.smithyql.parser.v3.Demo.Schema.Terminal
import playground.smithyql.parser.v3.Yikes.NamespaceContext
import playground.smithyql.parser.v3.Yikes.Source_fileContext
import playground.smithyql.parser.v3.Yikes.Use_clauseContext

import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag

case class SourceFile[F[_]](
  clauses: F[List[F[UseClause[F]]]]
) {

  def sequence(
    implicit F: Parallel[F],
    M: Monad[F],
  ): F[SourceFile[Id]] = clauses.flatMap {
    _.parTraverse(_.flatMap(_.sequence))
      .map(SourceFile[Id](_))
  }

}

case class UseClause[F[_]](
  namespace: F[NonEmptyList[F[String]]],
  service: F[String],
) {

  def sequence(
    implicit F: Parallel[F],
    M: Monad[F],
  ): F[UseClause[Id]] = (namespace.flatMap(_.parSequence), service)
    .parMapN(UseClause[Id](_, _))

}

object Demo {

  implicit class NullableOps[T](
    t: T
  ) {

    def requireOr(
      msg: String
    ): EitherNel[String, T] = Option(t).toRightNel(msg)

  }

  def main(
    args: Array[String]
  ): Unit = {
    val input =
      """
      use
      use service ..#
      use service x#
      use service a#oho
      use service b
      use service #foo
      """.stripMargin

    def tokenize(
      s: String
    ) = {
      val l = new Tokens(CharStreams.fromString(s))

      val p = new Yikes(new CommonTokenStream(l))
      p.removeErrorListeners()

      p
    }

    // val visitor =
    //   new YikesBaseVisitor[EitherNel[String, Any]] {
    //     override def visitSource_file(
    //       ctx: Source_fileContext
    //     ): EitherNel[String, SourceFile[EitherNel[String, *]]] = ctx
    //       .use_clause()
    //       .requireOr("no use clauses")
    //       .map(_.asScala.toList.map(visitUse_clause(_)))
    //       .map(SourceFile(_))

    //     override def visitUse_clause(
    //       ctx: Use_clauseContext
    //     ): EitherNel[String, UseClause[EitherNel[String, *]]] = ctx
    //       .qualified_identifier()
    //       .requireOr("missing qualified identifier")
    //       .flatMap { qi =>
    //         UseClause[EitherNel[String, *]](
    //           namespace = NonEmptyList
    //             .fromList(
    //               qi.namespace()
    //                 .ID()
    //                 .asScala
    //                 .toList
    //                 .map(_.requireOr("invalid namespace segment").map(_.getText()))
    //             )
    //             .toRightNel("missing namespace"),
    //           service = qi.ID().requireOr("missing ident node").map(_.getText()),
    //         ).asRight
    //       }

    //     override protected def defaultResult(
    //     ): EitherNel[String, Nothing] = sys.error("unsupported branch")
    //   }

    var errors: List[String] = Nil

    val p = tokenize(input)

    p.addErrorListener(new BaseErrorListener {

      override def syntaxError(
        recognizer: Recognizer[_ <: Object, _ <: Object],
        offendingSymbol: Object,
        line: Int,
        charPositionInLine: Int,
        msg: String,
        e: RecognitionException,
      ): Unit = {

        val (beforeError, afterError) = input
          .linesIterator
          .toList(line - 1)
          .splitAt(charPositionInLine)

        val previousLinesRange: String =
          input.linesWithSeparators.toList.slice(line - 3, line - 1).mkString

        val nextLinesRange: String = input.linesWithSeparators.toList.slice(line, line + 2).mkString

        errors =
          errors :+ (
            s"""ERROR $line:$charPositionInLine @ $msg
               |${previousLinesRange}${Console.GREEN}${beforeError}${Console.RED}${afterError}${Console.RESET}
               |${" " * charPositionInLine}^HERE${nextLinesRange}""".stripMargin
          )
      }

    })

    // val resultVisitor = visitor
    //   .visitSource_file(
    //     p.source_file()
    //   )

    // errors.foreach(println)

    // println(resultVisitor)
    // println(resultVisitor.flatMap(_.sequence))

    // p.reset()
    // p.removeErrorListeners()

    // def parseManual(
    //   p: Yikes
    // ): EitherNel[String, SourceFile[EitherNel[String, *]]] = p
    //   .source_file()
    //   .requireOr("no source file")
    //   .map { sf =>
    //     SourceFile {
    //       sf
    //         .use_clause()
    //         .asScala
    //         .toList
    //         .map { useClause =>
    //           UseClause(
    //             namespace = useClause
    //               .qualified_identifier()
    //               .namespace()
    //               .ID()
    //               .asScala
    //               .toList
    //               .map(_.requireOr("invalid namespace segment").map(_.getText()))
    //               .toNel
    //               .toRightNel("missing namespace"),
    //             service = useClause
    //               .qualified_identifier()
    //               .ID()
    //               .requireOr("missing ident node")
    //               .map(_.getText()),
    //           ).asRight
    //         }
    //     }
    //   }

    trait R[A] {
      protected def underlying: EitherNel[String, A]

      def flatMap[B](
        f: A => R[B]
      ): R[B] = R.wrap(underlying.flatMap(a => f(a).underlying))

      def map[B](
        f: A => B
      ): R[B] = flatMap(a => R.pure(f(a)))

      override def toString(
      ): String = underlying.toString()
    }

    object R {
      private def wrap[A](
        e: EitherNel[String, A]
      ): R[A] =
        new R[A] {
          protected def underlying: EitherNel[String, A] = e
        }

      def pure[A](
        a: A
      ): R[A] = wrap(Right(a))

      def required[A](
        a: A
      ): R[A] = {
        assert(a != null, "required value is null")
        pure(a)
      }

      def nullable[A](
        a: A
      ): R[Option[A]] = pure(Option(a))

      def collection[A](
        as: java.util.Collection[A]
      ): R[List[A]] = {
        assert(as != null, "required collection is null")
        pure(as.asScala.toList)
      }

      implicit class ROptionOps[A](
        ro: R[Option[A]]
      ) {
        def ifNull(
          msg: String
        ): R[A] = ro.flatMap {
          case Some(a) => R.pure(a)
          case None    => R.wrap(Left(NonEmptyList.one(msg)))
        }
      }

      implicit class RListOps[A](
        rl: R[List[A]]
      ) {
        def mapEach[B](
          f: A => B
        ): R[List[B]] = rl.map(_.map(f))

        def nonEmptyOr(
          msg: String
        ): R[NonEmptyList[A]] = rl.map(_.toNel).ifNull(msg)
      }

      def parAp: Applicative[R] =
        new Applicative[R] {
          def pure[A](
            x: A
          ): R[A] = R.pure(x)

          def ap[A, B](
            ff: R[A => B]
          )(
            fa: R[A]
          ): R[B] = wrap(
            Parallel.parAp(ff.underlying)(fa.underlying)
          )
        }

      implicit val mon: Monad[R] =
        new StackSafeMonad[R] {
          def flatMap[A, B](
            fa: R[A]
          )(
            f: A => R[B]
          ): R[B] = fa.flatMap(f)

          def pure[A](
            x: A
          ): R[A] = R.pure(x)
        }

      implicit val par: Parallel[R] =
        new Parallel[R] {
          type F[A] = R[A]

          def applicative: Applicative[R] = parAp
          def monad: Monad[R] = mon

          def parallel: FunctionK[R, R] = FunctionK.id
          def sequential: FunctionK[R, R] = FunctionK.id
        }
    }

    def nullCheck[A](
      name: String
    )(
      a: A
    ): A = { assert(a != null, s"$name is null"); a }

    // rules of thumb: ID can always be null if singular
    // def parseManual2(
    //   p: Yikes
    // ): R[SourceFile[R]] = R
    //   .nullable {
    //     p.source_file()
    //   }
    //   .ifNull("no source file")
    //   .flatMap { sf =>
    //     R.collection(sf.use_clause())
    //       .mapEach { useClause =>
    //         UseClause(
    //           namespace = R
    //             .nullable(useClause.qualified_identifier())
    //             .ifNull("no qualified identifier")
    //             .flatMap(qi =>
    //               R.collection(qi.namespace().ID())
    //                 .mapEach(R.required(_).map(_.getText()))
    //             )
    //             .nonEmptyOr("no namespace"),
    //           service = R
    //             .nullable(useClause.qualified_identifier())
    //             .ifNull("no qualified identifier")
    //             .map(_.ID())
    //             .map(_.getText()),
    //         )
    //       }
    //   }
    //   .map(_.map(R.pure(_)))
    //   .map(SourceFile(_))

    object schemas {

      val id: Schema[XR[String]] = Schema.token(Tokens.ID)
      val namespace: Schema[XR[NonEmptyList[XR[String]]]] = id
        .repeat1
        .at[Yikes.NamespaceContext]
        .map(_.flatten)

      val qi = Schema
        .struct2(
          namespace,
          id,
        )(UseClause.apply[XR])
        .at[Yikes.Qualified_identifierContext]
        .at[Yikes.Use_clauseContext]
        .map(_.flatten)

      val file: Schema[XR[SourceFile[XR]]] = Schema
        .struct1(
          qi.repeat
        )(SourceFile.apply[XR])
        .at[Yikes.Source_fileContext]

    }
    pprint.pprintln(schemas.file)

    println(decode(schemas.file, p))
    // val resultManual = parseManual(p)
    // println(resultVisitor == resultManual)

    // println(parseManual2(tokenize("use foo.bar#baz")))
    // println(parseManual2(tokenize("use foo.bar#baz")).map(_.sequence))
    // println(tokenize("").source_file().use_clause().asScala.toList.map(_.getText()))
  }

  trait XR[A] {

    def map[B](
      f: A => B
    ): XR[B] = ???

    def flatten[B](
      implicit ev: A <:< XR[B]
    ): XR[B] = ???

  }

  object XR {

    def pure[A](
      a: A
    ): XR[A] = ???

  }

  type StringK = XR[String]

  sealed trait Schema[Alg] {

    def map[A](
      f: Alg => A
    ): Schema[A] = Schema.Mapped(this, f)

    def repeat1: Schema[XR[NonEmptyList[Alg]]] = Schema.Repeat1(this)
    def repeat: Schema[XR[List[Alg]]] = Schema.Repeat(this)

    def at[A: ClassTag]: Schema.Field[XR[Alg]] = Schema.Node(
      this,
      implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[_ <: ParserRuleContext]],
    )

  }

  object Schema {

    case class Terminal(
      token: Int
    ) extends Schema[StringK]

    case class Repeat[Alg](
      item: Schema[Alg]
    ) extends Schema[XR[List[Alg]]]

    case class Repeat1[Alg](
      item: Schema[Alg]
    ) extends Schema[XR[NonEmptyList[Alg]]]

    type Field[Alg] = Schema[Alg]

    case class Struct[Alg](
      fields: List[Field[Any]],
      // fields: List[Schema[Any]],
      make: List[Any] => Alg,
    ) extends Schema[Alg]

    case class Node[Alg](
      schema: Schema[Alg],
      tag: Class[_ <: ParserRuleContext],
    ) extends Schema[XR[Alg]]

    case class Mapped[A, B](
      schema: Schema[A],
      f: A => B,
    ) extends Schema[B]

    def token(
      tok: Int
    ): Schema[StringK] = Terminal(tok)

    // TODO param for kind
    def struct1[Alg, A1](
      a1: Field[A1]
    )(
      f: A1 => Alg
    ): Schema[Alg] = Struct(
      List(a1).asInstanceOf[List[Field[Any]]],
      items => f(items(0).asInstanceOf[A1]),
    )

    // TODO param for kind
    def struct2[Alg, A1, A2](
      a1: Field[A1],
      a2: Field[A2],
    )(
      f: (
        A1,
        A2,
      ) => Alg
    ): Schema[Alg] = Struct(
      List(a1, a2).asInstanceOf[List[Field[Any]]],
      items =>
        f(
          items(0).asInstanceOf[A1],
          items(1).asInstanceOf[A2],
        ),
    )

  }

  def decode[A](
    schema: Schema[A],
    parser: Yikes,
  ): A =
    schema match {
      case n: Node[x] =>
        parser.source_file() match {
          case node if n.tag.isInstance(node) => XR.pure(decodeInner(n.schema, node))
        }
    }

  def decodeInner[A](
    schema: Schema[A],
    node: ParserRuleContext,
  ): A =
    schema match {
      case s: Struct[aa] =>
        def handleField[X](
          f: Schema.Field[X]
        ): X = {
          f match {
            case rrrr: Repeat[a1] =>
              XR.pure {
                rrrr.item match {
                  case m: Mapped[a2, b2] =>
                    m.f {
                      schema match {
                        case n: Node[a3] =>
                          Option(node.getRuleContexts(n.tag)) match {
                            case None        => ???
                            case Some(items) => items.asScala.toList.asInstanceOf[a2]
                          }
                      }
                    }
                }
              }
          }
        }.asInstanceOf[X]

        // sys.error(f.toString()) /* node.getRuleContexts() */

        val allFieldsDecoded = s.fields.map(handleField(_))

        s.make(allFieldsDecoded)
    }

}
