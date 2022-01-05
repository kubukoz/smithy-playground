package playground

import cats.Apply
import cats.Defer
import cats.implicits._
import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Parser0
import cats.parse.Rfc5234
import playground._
import cats.Id
import playground.AST.high._
import playground.AST.Token
import playground.AST.ALiteral
import util.chaining._
import playground.AST.WithSource

object SmithyQLParser {

  def parse(s: String): Either[ParsingFailure, Query[Id]] = parser
    .parseAll(s)
    .leftMap(ParsingFailure(_, s))
    .map(_.mapK(WithSource.unwrap))

  case class ParsingFailure(underlying: Parser.Error, text: String) extends Exception {

    def msg: String = {
      val (valid, failed) = text.splitAt(
        underlying.failedAtOffset
      )

      s"$valid${Console.RED}$failed${Console.RESET} - expected ${underlying.expected.map(_.toString()).mkString_(", ")}"
    }

  }

  val parser: Parser[Query[WithSource]] = {

    type T[+A] = WithSource[A]

    object tokens {
      import Parser._
      val comment: Parser[Token.Comment] =
        string("//") *> charsWhile0(_ != '\n').map(Token.Comment(_)) <* char('\n')

      val whitespace: Parser0[List[Token.Comment]] = comment
        .repSep0(charsWhile0(_.isWhitespace))
        .surroundedBy(charsWhile0(_.isWhitespace))

      def token[A](
        p: Parser[T[A]]
      ): Parser[T[A]] = (whitespace.with1 ~ p ~ whitespace).map { case ((before, v), after) =>
        WithSource(v.value, before ++ v.tokens ++ after)
      }

      val identifier: Parser[WithSource[String]] =
        (Rfc5234.alpha ~ Parser.charsWhile0(_.isLetterOrDigit))
          .map { case (ch, s) =>
            val wholeString = s.prepended(ch)
            WithSource(wholeString, Token.Identifier(wholeString) :: Nil)
          }

      val number = Numbers
        .digits
        .map { digits =>
          WithSource(
            IntLiteral(WithSource(digits.toInt, Token.Literal(ALiteral.IntLiteral(digits)) :: Nil)),
            Nil,
          )
        }

      val stringLiteral = anyChar
        .repUntil0(char('\"'))
        .map(_.mkString)
        .with1
        .surroundedBy(char('"'))
        .map { s =>
          WithSource(
            StringLiteral(WithSource(s, Token.Literal(ALiteral.StringLiteral(s)) :: Nil)),
            Nil,
          )
        }

      def charToken(c: Char)(tok: Token): Parser[T[Unit]] = token(
        char(c).as(WithSource((), tok :: Nil))
      )

      val equalsSign = charToken('=')(Token.EqualsSign)

      val comma = charToken(',')(Token.Comma)

      val openBrace = charToken('{')(Token.LeftBrace)
      val closeBrace = charToken('}')(Token.RightBrace)

    }

    import Parser._

    import tokens.token

    val ident: Parser[T[String]] = token(tokens.identifier)

    lazy val ast: Parser[T[AST[T]]] = Parser.defer(
      intLiteral.widen |
        stringLiteral.widen |
        struct.widen
    )

    lazy val intLiteral: Parser[T[IntLiteral[T]]] = token(tokens.number)

    // todo: allow quotes inside
    lazy val stringLiteral: Parser[T[StringLiteral[T]]] = token(tokens.stringLiteral)

    lazy val struct: Parser[T[Struct[T]]] = {
      type TField = (T[String], T[AST.high.AST[T]])

      val field: Parser[TField] = (ident, token(tokens.equalsSign), ast).mapN { (k, eqq, v) =>
        (k <* eqq, v)
      }

      // field, then optional whitespace, then optional coma, then optionally more `fields`
      val fields: Parser0[List[(T[String], T[AST[T]])]] = Defer[Parser0]
        .fix[List[TField]] { self =>
          val moreFields: Parser0[List[TField]] = (tokens.comma *> self).orElse(Parser.pure(Nil))

          (field ~ moreFields)
            .map { case (h, t) => h :: t }
            .orElse(Parser.pure(Nil))
        }

      (
        tokens.openBrace ~
          fields ~
          tokens.closeBrace
      ).map { case ((open, fieldsR), close) =>
        val fieldsResult =
          fieldsR match {
            case Nil    => Map.empty[T[String], T[AST[T]]]
            case fields => fields.toMap
          }

        open *>
          WithSource(Struct[T](WithSource(fieldsResult, Nil)), Nil) <* close
      }
    }

    (ident, struct).mapN(Query.apply[T])
  }

}
