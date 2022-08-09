package playground.smithyql

import cats.Id
import cats.implicits._
import cats.kernel.Eq
import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Parser.Expectation.InRange
import cats.parse.Parser0
import cats.parse.Rfc5234
import cats.Defer

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

      s"$valid${Console.RED}$failed${Console.RESET} - expected ${underlying
          .expected
          .map(showExpectation)
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

    val identifier: Parser[String] =
      (Rfc5234.alpha ~ Parser.charsWhile0(ch => ch.isLetterOrDigit || "_".contains(ch)))
        .map { case (ch, s) => s.prepended(ch) }

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
        rawIdent.repSep(tokens.dot.surroundedBy(tokens.whitespace)),
        tokens.hash *> rawIdent.surroundedBy(tokens.whitespace),
      ).mapN(QualifiedIdentifier.apply)

    val useClause: Parser[UseClause] = {
      string("use") *>
        string("service")
          .surroundedBy(tokens.whitespace) *>
        qualifiedIdent
    }.map(UseClause.apply)

    val intLiteral = tokens
      .number
      .map(IntLiteral[T](_))

    val boolLiteral = tokens.bool.map(BooleanLiteral[T](_))

    val stringLiteral = tokens.stringLiteral.map(StringLiteral[T](_))

    lazy val node: Parser[InputNode[T]] = Parser.defer {
      intLiteral |
        boolLiteral |
        stringLiteral |
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

    lazy val struct: Parser[Struct[T]] = {
      type TField = (T[Struct.Key], T[InputNode[T]])

      val field: Parser[TField] =
        (
          // sussy backtrack, but it works
          ident.map(_.map(Struct.Key.apply)).backtrack <* tokens.equalsSign,
          tokens.withComments(node),
        ).tupled

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
    lazy val listed: Parser[Listed[T]] = {
      type TField = T[InputNode[T]]

      val field: Parser[TField] = tokens.withComments(node)

      // field, then optional whitespace, then optional coma, then optionally more `fields`
      val fields: Parser0[List[TField]] = trailingCommaSeparated0(field.backtrack)

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

    val useClauseWithSource: Parser0[Option[WithSource[UseClause]]] =
      (tokens.comments ~
        Parser.index ~ useClause ~ Parser.index).backtrack.?.map {
        _.map { case (((commentsBefore, indexBefore), useClause), indexAfter) =>
          WithSource(
            commentsBefore,
            Nil,
            SourceRange(Position(indexBefore), Position(indexAfter)),
            useClause,
          )
        }
      }

    (useClauseWithSource.with1 ~
      ident.map(_.map(OperationName(_))) ~ struct ~ tokens.comments)
      .map { case (((useClause, opName), input), commentsAfter) =>
        Query(
          useClause,
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
