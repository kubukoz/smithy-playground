package playground.smithyql.parser.v3

import cats.Monad
import cats.Parallel
import cats.data.EitherNel
import cats.data.NonEmptyList
import cats.implicits._
import org.antlr.v4.runtime.BaseErrorListener
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.RecognitionException
import org.antlr.v4.runtime.Recognizer
import org.antlr.v4.runtime.tree.ErrorNode
import org.antlr.v4.runtime.tree.TerminalNode
import playground.smithyql.parser.v3.Yikes.Source_fileContext
import playground.smithyql.parser.v3.Yikes.Use_clauseContext

import scala.jdk.CollectionConverters._

object Demo {

  def main(
    args: Array[String]
  ): Unit = {
    val input =
      """|use
         |use service ..#
         |use service x#
         |use service a#oho
         |use service b
         |use service #foo""".stripMargin

    val l = new Tokens(CharStreams.fromString(input))

    val p = new Yikes(new CommonTokenStream(l))

    case class SourceFile[F[_]](
      clauses: List[F[UseClause[F]]]
    ) {

      def sequence(
        implicit F: Parallel[F],
        M: Monad[F],
      ): F[SourceFile[cats.Id]] = clauses
        .parTraverse(_.flatMap(_.sequence))
        .map(SourceFile[cats.Id](_))

    }

    case class UseClause[F[_]](
      namespace: F[NonEmptyList[F[String]]],
      service: F[String],
    ) {

      def sequence(
        implicit F: Parallel[F],
        M: Monad[F],
      ): F[UseClause[cats.Id]] = (namespace.flatMap(_.parSequence), service)
        .parMapN(UseClause[cats.Id](_, _))

    }

    implicit class NullableOps[T](
      t: T
    ) {

      def requireOr(
        msg: String
      ): EitherNel[String, T] = Option(t).toRightNel(msg)

    }

    def checkTerminal(
      p: TerminalNode
    ): EitherNel[String, String] = p.accept(
      new YikesBaseVisitor[EitherNel[String, String]] {
        override protected def defaultResult(
        ): EitherNel[String, String] = sys.error("unsupported")

        override def visitTerminal(
          node: TerminalNode
        ): EitherNel[String, String] = node.getText().asRight

        override def visitErrorNode(
          node: ErrorNode
        ): EitherNel[String, String] = s"error node: ${node.getText()}".leftNel
      }
    )

    def parseFull(
      p: Yikes
    ): EitherNel[String, SourceFile[EitherNel[String, *]]] = p
      .source_file()
      .requireOr("no source file")
      .map { sf =>
        SourceFile[EitherNel[String, *]](
          sf
            .use_clause()
            .asScala
            .toList
            .map { useClause =>
              UseClause[EitherNel[String, *]](
                namespace = NonEmptyList
                  .fromList(
                    useClause
                      .qualified_identifier()
                      .namespace()
                      .ID()
                      .asScala
                      .toList
                      .map(_.requireOr("invalid namespace segment").flatMap(checkTerminal(_)))
                  )
                  .toRightNel("missing namespace"),
                service = useClause
                  .qualified_identifier()
                  .ID()
                  .requireOr("missing ident node")
                  .flatMap {
                    checkTerminal
                  },
              ).asRight
            }
        )
      }

    p.removeErrorListeners()

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

        println(
          s"""ERROR $line:$charPositionInLine @ $msg
             |${previousLinesRange}${Console.GREEN}${beforeError}${Console.RED}${afterError}${Console.RESET}
             |${" " * charPositionInLine}^HERE${nextLinesRange}""".stripMargin
        )
      }

    })

    val r = parseFull(p)
    println("parsed N rules: " + r.toOption.get.clauses.size)
    r.toOption.get.clauses.foreach(println)
    println("result: " + r.map(_.sequence))

    p.reset()
    p.removeErrorListeners()
    val r2 =
      new YikesBaseVisitor[EitherNel[String, Any]] {
        override def visitSource_file(
          ctx: Source_fileContext
        ): EitherNel[String, SourceFile[EitherNel[String, *]]] = ctx
          .use_clause()
          .requireOr("no use clauses")
          .map {
            _.asScala.toList.map(visitUse_clause(_))
          }
          .map(SourceFile(_))

        override def visitUse_clause(
          ctx: Use_clauseContext
        ): EitherNel[String, UseClause[EitherNel[String, *]]] = ctx
          .qualified_identifier()
          .requireOr("missing qualified identifier")
          .flatMap { qi =>
            UseClause[EitherNel[String, *]](
              namespace = NonEmptyList
                .fromList(
                  qi.namespace()
                    .ID()
                    .asScala
                    .toList
                    .map(_.requireOr("invalid namespace segment").flatMap(checkTerminal(_)))
                )
                .toRightNel("missing namespace"),
              service = qi.ID().requireOr("missing ident node").flatMap {
                checkTerminal
              },
            ).asRight
          }

        override protected def defaultResult(
        ): EitherNel[String, Nothing] = sys.error("unsupported branch")
      }
        .visitSource_file(
          p.source_file()
        )

    println(r)
    println(r2)
    println(r == r2)

    println(r2.flatMap(_.sequence))
  }

}
