package playground

import cats.Id
import cats.implicits._
import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Parser.Expectation.InRange
import cats.parse.Parser0
import cats.parse.Rfc5234
import playground.AST.Comment
import playground.AST.WithSource
import playground.AST.high._
import playground._

object SmithyQLParser {

  def parse(s: String): Either[ParsingFailure, Query[Id]] = parser
    .parseAll(s)
    .leftMap(ParsingFailure(_, s))
    .map(_.map(_.mapK(WithSource.unwrap)).value)

  case class ParsingFailure(underlying: Parser.Error, text: String) extends Exception {

    def msg: String = {
      val (valid, failed) = text.splitAt(
        underlying.failedAtOffset
      )

      s"$valid${Console.RED}$failed${Console.RESET} - expected ${underlying
        .expected
        .map {
          case InRange(_, '0', '9')               => "digit"
          case InRange(_, from, to) if from == to => s"'$from'"
          case InRange(_, from, to)               => s"'$from' - '$to'"
          case e                                  => e.toString
        }
        .mkString_("/")} at offset ${underlying.failedAtOffset}, got ${Console.YELLOW}\"${failed
        .take(10)}\"${Console.RESET} instead"
    }

  }

  private type T[+A] = WithSource[A]

  object tokens {
    import Parser._
    val comment: Parser[Comment] =
      string("//") *> charsWhile0(_ != '\n').map(Comment(_)) <* char('\n')

    val whitespace: Parser0[Unit] = charsWhile0(_.isWhitespace).void

    val comments: Parser0[List[Comment]] = comment
      .repSep0(whitespace)
      .surroundedBy(whitespace)

    def withComments[A](
      p: Parser[A]
    ): Parser[T[A]] = (comments.with1 ~ p ~ comments).map { case ((b, v), a) =>
      WithSource(b, v, a)
    }

    private[SmithyQLParser] val rawIdentifier =
      (Rfc5234.alpha ~ Parser.charsWhile0(_.isLetterOrDigit))
        .map { case (ch, s) => s.prepended(ch) }

    val identifier: Parser[T[String]] = withComments {
      (Rfc5234.alpha ~ Parser.charsWhile0(_.isLetterOrDigit))
        .map { case (ch, s) => s.prepended(ch) }
    }

    val number: Parser[Int] = Numbers
      .digits
      .map(_.toInt)

    // todo: allow quotes inside
    val stringLiteral: Parser[String] = anyChar
      .repUntil0(char('\"'))
      .map(_.mkString)
      .with1
      .surroundedBy(char('"'))

    def punctuation(c: Char): Parser[Unit] = char(c)

    val equalsSign = punctuation('=')
    val comma = punctuation(',')
    val openBrace = punctuation('{')
    val closeBrace = punctuation('}')

  }

  val parser: Parser[WithSource[Query[WithSource]]] = {

    import Parser._

    val ident: Parser[T[String]] = tokens.identifier

    lazy val node: Parser[InputNode[T]] = Parser.defer {

      // Consuming comments/whitespace for nodes together to avoid double backtracking.
      tokens
        .withComments {
          tokens.number.backtrack.eitherOr(tokens.stringLiteral).eitherOr(struct)
        }
        .map { soi => // boi
          soi
            .value
            .fold(
              s => Struct(s.fields.between(soi.tokensLeft, soi.tokensRight)),
              _.fold(
                s => StringLiteral(soi.copy(value = s)),
                i => IntLiteral(soi.copy(value = i)),
              ),
            )
        }
    }

    lazy val struct: Parser[Struct[T]] = {
      type TField = (T[String], AST.high.InputNode[T])

      val field: Parser[TField] =
        (
          // sussy backtrack, but it works
          ident.backtrack <* tokens.equalsSign,
          node,
        ).tupled

      // field, then optional whitespace, then optional coma, then optionally more `fields`
      lazy val fields: Parser0[List[TField]] = Parser.defer0 {
        val moreFields: Parser0[List[TField]] = (tokens.comma *> fields).orElse(Parser.pure(Nil))

        (field ~ moreFields)
          .map { case (h, t) => h :: t }
          .orElse(Parser.pure(Nil))
      }

      // No comments around opening brace
      tokens.openBrace *>
        (
          // fields always start with whitespace/comments, so we don't catch that here
          fields ~
            tokens.comments <*
            tokens.closeBrace
        ).map { case (fieldsR, commentsBeforeEnd) =>
          val fieldsResult =
            fieldsR match {
              case Nil    => Map.empty[T[String], InputNode[T]]
              case fields => fields.toMap
            }

          Struct[T](WithSource(Nil, fieldsResult, commentsBeforeEnd))
        }
    }

    {
      (ident, struct).mapN(Query.apply[T]) ~ tokens.comments
    }.map { case (query, after) => WithSource(Nil, query, after) }
  }

}