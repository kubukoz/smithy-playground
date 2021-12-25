package playground

import cats.Defer
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
          s"$valid${Console.RED}$failed${Console.RESET} - expected ${e.expected.map(_.toString()).mkString_(", ")}"
        ) with NoStackTrace
      }
      .merge

  object tokens {
    import Parser._

    val symbol: Parser[String] = (Rfc5234.alpha ~ Parser.charsWhile0(_.isLetterOrDigit)).map {
      case (ch, s) => s.prepended(ch)
    }

    val number = Numbers.digits.map(_.toInt).map(IntLiteral)

    val stringLiteral = anyChar
      .repUntil0(char('\"'))
      .map(_.mkString)
      .with1
      .surroundedBy(char('"'))
      .map(StringLiteral)

    val equalsSign = char('=')

    val comma = char(',')

    val openBrace = char('{')
    val closeBrace = char('}')

    val comment = string("//") *> charsWhile0(_ != '\n') *> char('\n')
  }

  val parser: Parser[Query] = {
    import Parser._

    val singleLineComment = tokens.comment

    val optionalWhitespace: Parser0[Unit] =
      singleLineComment
        .repSep0(charsWhile0(_.isWhitespace))
        .surroundedBy(charsWhile0(_.isWhitespace))
        .void

    def token[A](p: Parser[A]): Parser[A] = p.surroundedBy(optionalWhitespace)

    val symbol: Parser[String] = token(tokens.symbol)

    lazy val ast: Parser[AST] = Parser.defer(intLiteral | stringLiteral | struct)

    lazy val intLiteral: Parser[IntLiteral] = token(tokens.number)

    // todo: allow quotes inside
    lazy val stringLiteral: Parser[StringLiteral] = token(tokens.stringLiteral)

    lazy val struct: Parser[Struct] = {
      val field: Parser[(String, AST)] =
        symbol ~ (
          token(tokens.equalsSign) *>
            ast
        )

      // field, then optional whitespace, then optional coma, then optionally more `fields`
      val fields: Parser0[List[(String, AST)]] = Defer[Parser0].fix[List[(String, AST)]] { self =>
        val empty = Parser.pure(Nil)

        (
          field,
          (token(tokens.comma) *> self).orElse(empty),
        )
          .mapN((first, rest) => first :: rest)
          .orElse(empty)

      }

      fields
        .map(_.toMap)
        .with1
        .between(
          token(tokens.openBrace),
          token(tokens.closeBrace),
        )
        .map(Struct(_))
    }

    (symbol, struct).mapN(Query.apply)
  }

}
