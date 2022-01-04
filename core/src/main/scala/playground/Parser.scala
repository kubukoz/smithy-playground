package playground

import cats.Apply
import cats.Defer
import cats.implicits._
import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Parser0
import cats.parse.Rfc5234
import playground._

object SmithyQLParser {

  val parse: String => Either[ParsingFailure, Query] = {

    val theParser = parser(Tokens.idTokens)

    s =>
      theParser
        .parseAll(s)
        .leftMap(ParsingFailure(_, s))
  }

  case class ParsingFailure(underlying: Parser.Error, text: String) extends Exception {

    def msg: String = {
      val (valid, failed) = text.splitAt(
        underlying.failedAtOffset
      )

      s"$valid${Console.RED}$failed${Console.RESET} - expected ${underlying.expected.map(_.toString()).mkString_(", ")}"
    }

  }

  val idParser = parser(Tokens.idTokens)

  def parser[F[_]: Apply](T: Tokens[F]): Parser[F[Query]] = {

    object tokens {
      import Parser._
      val comment = string("//") *> charsWhile0(_ != '\n').map(T.comment) <* char('\n')

      val whitespace: Parser0[Option[F[Unit]]] = comment
        .repSep0(charsWhile0(_.isWhitespace))
        .surroundedBy(charsWhile0(_.isWhitespace))
        .map(_.toNel.map(_.nonEmptySequence_))

      def token[A](
        p: Parser[F[A]]
      ): Parser[F[A]] = (whitespace.with1 ~ p ~ whitespace).map {
        // trust me, this is the only readable way
        case ((Some(a), b), Some(c)) => a *> b <* c
        case ((Some(a), b), None)    => a *> b
        case ((None, b), Some(c))    => b <* c
        case ((None, b), None)       => b
      }

      val symbol: Parser[F[String]] = T.liftParser {
        (Rfc5234.alpha ~ Parser.charsWhile0(_.isLetterOrDigit)).map { case (ch, s) =>
          s.prepended(ch)
        }
      }

      val number = T.liftParser(Numbers.digits).map(_.map(_.toInt).map(IntLiteral))

      val stringLiteral = anyChar
        .repUntil0(char('\"'))
        .map(_.mkString)
        .with1
        .surroundedBy(char('"'))
        .map { s =>
          T.liftToken(s"\"$s\"", StringLiteral(s))
        }

      def charToken(c: Char): Parser[F[Unit]] = token(char(c).as(T.liftToken(c.toString, ())))

      val equalsSign = charToken('=')

      val comma = charToken(',')

      val openBrace = charToken('{')
      val closeBrace = charToken('}')

    }

    import Parser._

    import tokens.token

    val symbol: Parser[F[String]] = token(tokens.symbol)

    lazy val ast: Parser[F[AST]] = Parser.defer(
      intLiteral.map(_.widen[AST]).widen |
        stringLiteral.map(_.widen[AST]).widen |
        struct.map(_.widen[AST]).widen
    )

    lazy val intLiteral: Parser[F[IntLiteral]] = token(tokens.number)

    // todo: allow quotes inside
    lazy val stringLiteral: Parser[F[StringLiteral]] = token(tokens.stringLiteral)

    lazy val struct: Parser[F[Struct]] = {
      val field: Parser[F[(String, AST)]] = (symbol, token(tokens.equalsSign), ast).mapN {
        (k, eqq, v) => (k <* eqq, v).tupled
      }

      // field, then optional whitespace, then optional coma, then optionally more `fields`
      val fields: Parser0[Option[F[List[(String, AST)]]]] = Defer[Parser0]
        .fix[Option[F[List[(String, AST)]]]] { self =>
          val empty = Parser.pure(Nil)

          (
            field,
            (tokens.comma, self)
              .mapN { (comma, selfResult) =>
                selfResult match {
                  case Some(r) => comma *> r
                  case None    => comma.as(Nil)
                }
              }
              .eitherOr(empty),
          )
            .mapN { (lhs, rhs) =>
              rhs match {
                case Left(Nil) => lhs.map(List(_))
                case Right(v)  => (lhs, v).tupled.map { case (k, v) => k :: v }
              }
            }
            .map(Some(_))
            .orElse(empty.as(None))

        }

      (
        tokens.openBrace ~
          fields ~
          tokens.closeBrace
      ).map { case ((open, fieldsR), close) =>
        val fieldsResult =
          fieldsR match {
            case Some(f) => open *> f.map(_.toMap) <* close
            case None    => open *> close.as(Map.empty[String, AST])
          }

        fieldsResult.map(Struct(_))
      }.map(T.block(_))
    }

    (symbol, struct).mapN((_, _).mapN(Query.apply))
  }

}
