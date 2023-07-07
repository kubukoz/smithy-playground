package playground.smithyql.parser.v2

import cats.data.NonEmptyList
import org.antlr.v4.runtime.BaseErrorListener
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.RecognitionException
import org.antlr.v4.runtime.Recognizer
import org.antlr.v4.runtime.tree.ParseTree
import playground.smithyql.parser.v2.Yikes.PreludeContext
import playground.smithyql.parser.v2.Yikes.Source_fileContext
import playground.smithyql.parser.v2.Yikes.Use_clauseContext

import scala.jdk.CollectionConverters._

object Demo extends App {

  val input =
    """
    use service foo.bar#Hello
    use service foo#Hello2
    """.stripMargin

  val l = new Tokens(CharStreams.fromString(input))

  val p = new Yikes(new CommonTokenStream(l))

  case class UseClause(
    namespace: NonEmptyList[String],
    service: String,
  )

  case class Prelude(
    clauses: List[UseClause]
  )

  case class SourceFile(
    prelude: Prelude
  )

  def parseFull(
    p: Yikes
  ): SourceFile = SourceFile(
    Prelude(
      p
        .source_file()
        .prelude()
        .use_clause()
        .asScala
        .toList
        .map { useClause =>
          UseClause(
            namespace = NonEmptyList.fromListUnsafe(
              useClause
                .qualified_identifier()
                .namespace()
                .ID()
                .asScala
                .toList
                .map(_.getText)
            ),
            service = useClause.qualified_identifier().ID().getText(),
          )
        }
    )
  )

  val parseFullVis: YikesVisitor[List[UseClause]] =
    new YikesBaseVisitor[List[UseClause]] {
      override protected def defaultResult(
      ): List[UseClause] = Nil

      override def visitSource_file(
        ctx: Source_fileContext
      ): List[UseClause] = ctx.prelude().accept(this)

      override def visitPrelude(
        ctx: PreludeContext
      ): List[UseClause] = ctx.use_clause().asScala.toList.flatMap(_.accept(this))

      override def visitUse_clause(
        ctx: Use_clauseContext
      ): List[UseClause] = List(
        UseClause(
          namespace = NonEmptyList.fromListUnsafe(
            ctx
              .qualified_identifier()
              .namespace()
              .ID()
              .asScala
              .toList
              .map(_.getText)
          ),
          service = ctx.qualified_identifier().ID().getText(),
        )
      )

    }

  println(parseFull(p))

  p.reset()

  println(SourceFile(Prelude(p.source_file().accept(parseFullVis))))

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

}
