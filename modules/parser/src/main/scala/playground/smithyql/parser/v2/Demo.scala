package playground.smithyql.parser.v2

import cats.data.NonEmptyList
import org.antlr.v4.runtime.BaseErrorListener
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.RecognitionException
import org.antlr.v4.runtime.Recognizer

import scala.jdk.CollectionConverters._

object Demo extends App {

  val input =
    """
    use service a
    use service b
    use service #foo
    """.stripMargin

  val l = new Tokens(CharStreams.fromString(input))

  val p = new Yikes(new CommonTokenStream(l))

  case class SourceFile[F[_]](
    clauses: List[F[UseClause[F]]]
  )

  case class UseClause[F[_]](
    namespace: F[NonEmptyList[F[String]]],
    service: F[String],
  )

  implicit class NullableOps[T](
    t: T
  ) {
    def toOption: Option[T] = Option(t)
  }

  def parseFull(
    p: Yikes
  ): SourceFile[Option] = SourceFile[Option](
    p
      .source_file()
      .use_clause()
      .asScala
      .toList
      .map { useClause =>
        Some {
          UseClause(
            namespace = NonEmptyList.fromList(
              useClause
                .qualified_identifier()
                .namespace()
                .ID()
                .asScala
                .toList
                .map(_.toOption.map(_.getText))
            ),
            service = useClause.qualified_identifier().ID().toOption.map(_.getText()),
          )
        }
      }
  )

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

  parseFull(p).clauses.foreach(println)

  p.reset()

}
