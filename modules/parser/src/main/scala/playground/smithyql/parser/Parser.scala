package playground.smithyql.parser

import cats.Defer
import cats.Id
import cats.implicits._
import cats.kernel.Eq
import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Parser.Expectation.InRange
import cats.parse.Parser0
import cats.parse.Rfc5234
import playground.smithyql._

object SmithyQLParser {

  def parse(s: String): Either[ParsingFailure, Query[Id]] = parseFull(s)
    .map(_.mapK(WithSource.unwrap))

  def parseFull(s: String): Either[ParsingFailure, Query[WithSource]] = parser
    .parseAll(s)
    .leftMap(ParsingFailure(_, s))

  case class ParsingFailure(underlying: Parser.Error, text: String) extends Exception {

    override def getMessage: String = msg

    def msg: String = {
      val (valid, failed) = text.splitAt(
        underlying.failedAtOffset
      )

      def showExpectation(e: Parser.Expectation): String =
        e match {
          case InRange(_, '0', '9')               => "digit"
          case InRange(_, from, to) if from == to => s"'$from'"
          case InRange(_, from, to)               => s"'$from' - '$to'"
          case e                                  => e.toString
        }

      def prep(s: String): String = s.replace(' ', '·').replace("\n", "\\n\n")

      s"$valid${Console.RED}$failed${Console.RESET} - expected ${underlying
          .expected
          .map(showExpectation)
          .mkString_("/")} after ${Console.BLUE}${prep(
          text.take(
            underlying.failedAtOffset
          )
        )}${Console.RESET}, got ${Console.YELLOW}\"${prep(
          failed
            .take(10)
        )}\"${Console.RESET} instead"
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
    ): Parser[T[A]] = ((comments ~ Parser.index).soft.with1 ~ p ~ (Parser.index ~ comments)).map {
      case (((commentsBefore, indexBefore), v), (indexAfter, commentsAfter)) =>
        val range = SourceRange(Position(indexBefore), Position(indexAfter))
        WithSource(
          commentsLeft = commentsBefore,
          commentsRight = commentsAfter,
          range = range,
          value = v,
        )
    }

    def withComments0[A](
      p: Parser0[A]
    ): Parser0[T[A]] = ((comments ~ Parser.index).soft ~ p ~ (Parser.index ~ comments)).map {
      case (((commentsBefore, indexBefore), v), (indexAfter, commentsAfter)) =>
        val range = SourceRange(Position(indexBefore), Position(indexAfter))
        WithSource(
          commentsLeft = commentsBefore,
          commentsRight = commentsAfter,
          range = range,
          value = v,
        )
    }

    def withRange[A](
      p: Parser[A]
    ): Parser[T[A]] = (Parser.index.with1 ~ p ~ Parser.index).map {
      case ((indexBefore, v), indexAfter) =>
        val range = SourceRange(Position(indexBefore), Position(indexAfter))

        WithSource(
          commentsLeft = Nil,
          commentsRight = Nil,
          range = range,
          value = v,
        )
    }

    private[SmithyQLParser] val rawIdentifier =
      (Rfc5234.alpha ~ Parser.charsWhile0(_.isLetterOrDigit))
        .map { case (ch, s) => s.prepended(ch) }

    val identifier: Parser[String] =
      (Rfc5234.alpha ~ Parser.charsWhile0(ch => ch.isLetterOrDigit || "_".contains(ch)))
        .map { case (ch, s) => s.prepended(ch) }

    val number: Parser[String] = Numbers.jsonNumber

    val bool: Parser[Boolean] = string("true").as(true).orElse(string("false").as(false))

    // todo: allow quotes inside
    val stringLiteral: Parser[String] = anyChar
      .repUntil0(char('\"'))
      .map(_.mkString)
      .with1
      .surroundedBy(char('"'))

    val nullLiteral: Parser[Unit] = string("null")

    def punctuation(c: Char): Parser[Unit] = char(c)

    val equalsSign = punctuation('=')
    val dot = punctuation('.')
    val comma = punctuation(',')
    val hash = punctuation('#')
    val openBrace = punctuation('{')
    val closeBrace = punctuation('}')

    val openBracket = punctuation('[')
    val closeBracket = punctuation(']')

  }

  val parser: Parser[Query[WithSource]] = {

    import Parser._

    val rawIdent: Parser[String] = tokens.identifier
    val ident: Parser[T[String]] = tokens.withComments(tokens.identifier)

    // doesn't accept comments
    val qualifiedIdent: Parser[QualifiedIdentifier] =
      (
        rawIdent.repSep(tokens.dot.surroundedBy(tokens.whitespace).backtrack),
        tokens.hash.surroundedBy(tokens.whitespace) *> rawIdent,
      ).mapN(QualifiedIdentifier.apply)

    val useClause: Parser[UseClause[T]] = {
      string("use") *>
        string("service")
          .surroundedBy(tokens.whitespace) *>
        tokens.withRange(qualifiedIdent)
    }.map(UseClause.apply[T])

    val intLiteral = tokens.withComments {
      tokens
        .number
        .map(IntLiteral[T](_))
    }

    val boolLiteral = tokens.withComments(tokens.bool.map(BooleanLiteral[T](_)))

    val stringLiteral = tokens.withComments(tokens.stringLiteral.map(StringLiteral[T](_)))
    val nullLiteral = tokens.withComments(tokens.nullLiteral.map(_ => NullLiteral[T]()))

    lazy val node: Parser[T[InputNode[T]]] = Parser.defer {
      intLiteral |
        boolLiteral |
        stringLiteral |
        nullLiteral |
        struct |
        listed
    }

    def trailingCommaSeparated0[A](
      parser: Parser[A]
    ): Parser0[List[A]] = Defer[Parser0].fix[List[A]] { self =>
      val moreFields: Parser0[List[A]] = (tokens.comma *> self).orElse(
        Parser.pure(Nil)
      )

      (parser ~ moreFields)
        .map { case (h, t) => h :: t }
        .orElse(Parser.pure(Nil))
    }

    lazy val struct: Parser[T[Struct[T]]] = tokens.withComments {
      type TField = Binding[T]

      val field: Parser[TField] =
        (
          ident.map(_.map(Identifier.apply)) <* tokens.equalsSign,
          node,
        ).mapN(Binding.apply[T])

      // field, then optional whitespace, then optional coma, then optionally more `fields`
      val fields: Parser0[List[TField]] = trailingCommaSeparated0(field)

      tokens.openBrace *>
        (
          Parser.index ~
            // fields always start with whitespace/comments, so we don't catch that here
            fields ~
            tokens.comments ~
            (Parser.index <*
              tokens.closeBrace)
        ).map { case (((indexInside, fieldsR), commentsBeforeEnd), indexBeforeExit) =>
          val fieldsResult = Struct.Fields.fromSeq(fieldsR)

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

    // this is mostly copy-pasted from structs, might not work lmao
    lazy val listed: Parser[T[Listed[T]]] = tokens.withComments {
      type TField = T[InputNode[T]]

      val field: Parser[TField] = node

      // field, then optional whitespace, then optional coma, then optionally more `fields`
      val fields: Parser0[List[TField]] = trailingCommaSeparated0(field)

      tokens.openBracket *>
        (
          Parser.index ~
            // fields always start with whitespace/comments, so we don't catch that here
            fields ~
            tokens.comments ~
            (Parser.index <*
              tokens.closeBracket)
        ).map { case (((indexInside, fieldsR), commentsBeforeEnd), indexBeforeExit) =>
          val fieldsResult = fieldsR
          val range = SourceRange(Position(indexInside), Position(indexBeforeExit))

          Listed {
            WithSource(
              commentsLeft = Nil,
              commentsRight = commentsBeforeEnd,
              range = range,
              value = fieldsResult,
            )
          }
        }
    }

    val useClauseWithSource: Parser0[WithSource[Option[UseClause[WithSource]]]] = tokens
      .withComments0(useClause.?)

    val queryOperationName: Parser[T[QueryOperationName[WithSource]]] = {

      val serviceRef =
        tokens
          .withRange(
            qualifiedIdent.backtrack <* tokens.whitespace
          ) <* tokens.dot

      val operationName = tokens.withRange(rawIdent).map(_.map(OperationName[WithSource](_)))

      tokens.withComments {
        ((serviceRef <* tokens.whitespace).?.with1 ~ operationName).map {
          QueryOperationName.apply[WithSource].tupled
        }
      }
    }

    (useClauseWithSource.with1 ~ queryOperationName ~ struct).map {
      case ((useClause, opName), input) =>
        Query(
          useClause,
          opName,
          input,
        )
    }
  }

}
