package playground.smithyql

import cats.Id
import cats.implicits._
import cats.kernel.Eq
import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Parser.Expectation.InRange
import cats.parse.Parser0
import cats.parse.Rfc5234

object SmithyQLParser {

  def parse(s: String): Either[ParsingFailure, Query[Id]] = parseFull(s)
    .map(_.mapK(WithSource.unwrap))

  def parseFull(s: String): Either[ParsingFailure, Query[WithSource]] = parser
    .parseAll(s)
    .leftMap(ParsingFailure(_, s))

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

  object ParsingFailure {
    implicit val eq: Eq[ParsingFailure] = Eq.fromUniversalEquals
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
    ): Parser[T[A]] = ((comments ~ Parser.index).with1 ~ p ~ (Parser.index ~ comments)).map {
      case (((commentsBefore, indexBefore), v), (indexAfter, commentsAfter)) =>
        val range = SourceRange(Position(indexBefore), Position(indexAfter))
        WithSource(
          commentsLeft = commentsBefore,
          commentsRight = commentsAfter,
          range = range,
          value = v,
        )
    }

    private[SmithyQLParser] val rawIdentifier =
      (Rfc5234.alpha ~ Parser.charsWhile0(_.isLetterOrDigit))
        .map { case (ch, s) => s.prepended(ch) }

    val identifier: Parser[T[String]] = withComments {
      (Rfc5234.alpha ~ Parser.charsWhile0(_.isLetterOrDigit))
        .map { case (ch, s) => s.prepended(ch) }
    }

    val number: Parser[Int] = Numbers
      .signedIntString
      .map(_.toInt)

    val bool: Parser[Boolean] = string("true").as(true).orElse(string("false").as(false))

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

  val parser: Parser[Query[WithSource]] = {

    import Parser._

    val ident: Parser[T[String]] = tokens.identifier

    val intLiteral = tokens
      .number
      .map(IntLiteral[T](_))

    val boolLiteral = tokens.bool.map(BooleanLiteral[T](_))

    val stringLiteral = tokens.stringLiteral.map(StringLiteral[T](_))

    lazy val node: Parser[InputNode[T]] = Parser.defer {
      intLiteral |
        boolLiteral |
        stringLiteral |
        struct
    }

    lazy val struct: Parser[Struct[T]] = {
      type TField = (T[Struct.Key], T[InputNode[T]])

      val field: Parser[TField] =
        (
          // sussy backtrack, but it works
          ident.map(_.map(Struct.Key.apply)).backtrack <* tokens.equalsSign,
          tokens.withComments(node),
        ).tupled

      // field, then optional whitespace, then optional coma, then optionally more `fields`
      lazy val fields: Parser0[List[TField]] = Parser.defer0 {
        val moreFields: Parser0[List[TField]] = (tokens.comma *> fields).orElse(Parser.pure(Nil))

        (field ~ moreFields)
          .map { case (h, t) => h :: t }
          .orElse(Parser.pure(Nil))
      }

      tokens.openBrace *>
        (
          Parser.index ~
            // fields always start with whitespace/comments, so we don't catch that here
            fields ~
            tokens.comments ~
            (Parser.index <*
              tokens.closeBrace)
        ).map { case (((indexInside, fieldsR), commentsBeforeEnd), indexBeforeExit) =>
          val fieldsResult =
            fieldsR match {
              case Nil    => Struct.Fields.empty[T]
              case fields => Struct.Fields.fromSeq(fields)
            }

          val range = SourceRange(Position(indexInside), Position(indexBeforeExit))

          Struct {
            WithSource(
              commentsLeft = Nil,
              commentsRight = commentsBeforeEnd,
              range = range,
              value = fieldsResult,
            )
          }
        }
    }

    (ident.map(_.map(OperationName(_))) ~ struct ~ tokens.comments).map {
      case ((opName, input), commentsAfter) =>
        Query(
          opName,
          WithSource(
            commentsLeft = Nil,
            commentsRight = commentsAfter,
            range = input.fields.range,
            value = input,
          ),
        )
    }
  }

}
