package playground

import cats.implicits._
import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Parser0
import cats.parse.Rfc5234
import playground._

import scala.util.control.NoStackTrace

object SmithyQLParser {

  def parse(s: String) =
    parser
      .parseAll(s)
      .leftMap { e =>
        val (valid, failed) = s.splitAt(
          e.failedAtOffset
        )

        throw new Exception(
          s"$valid<<<FAIL>>>$failed - expected ${e.expected.map(_.toString()).mkString_(", ")}"
        ) with NoStackTrace
      }
      .merge

  val parser: Parser[Query] = {
    import Parser._

    val symbol: Parser[String] = (Rfc5234.alpha ~ Parser.charsWhile0(_.isLetterOrDigit)).map {
      case (ch, s) => s.prepended(ch)
    }

    val optionalWhitespace: Parser0[Unit] = charsWhile0(_.isWhitespace).void

    lazy val ast: Parser[AST] = Parser.defer(intLiteral | stringLiteral | struct)

    lazy val intLiteral: Parser[IntLiteral] = Numbers.digits.map(_.toInt).map(IntLiteral)

    // todo: allow quotes inside
    lazy val stringLiteral: Parser[StringLiteral] = anyChar
      .repUntil0(char('\"'))
      .map(_.mkString)
      .with1
      .surroundedBy(char('"'))
      .map(StringLiteral)

    lazy val struct: Parser[Struct] = {
      val field: Parser[(String, AST)] =
        symbol ~ (
          optionalWhitespace *>
            char('=') *>
            optionalWhitespace *>
            ast
        )

      // field, then optional whitespace, then optional coma, then optionally more `fields`
      lazy val fields: Parser0[List[(String, AST)]] = Parser
        .defer {
          field ~ {
            optionalWhitespace *>
              (char(',') *>
                optionalWhitespace *>
                fields).?
          }
        }
        .?
        .map {
          _.fold(Nil: List[(String, AST)]) { case (a, b) => a :: b.getOrElse(Nil) }
        }

      fields
        .surroundedBy(optionalWhitespace)
        .map(_.toMap)
        .with1
        .between(
          char('{'),
          char('}'),
        )
        .map(Struct(_))
    }

    (symbol, optionalWhitespace.with1 *> struct)
      .mapN(Query.apply)
      .surroundedBy(optionalWhitespace)
  }

}
