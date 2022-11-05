package playground.smithyql.parser

import cats.implicits._
import cats.kernel.Eq
import cats.parse.Parser
import cats.parse.Parser.Expectation.InRange
import cats.parse.Parser0
import playground.smithyql._
import cats.parse.Parser.Expectation.OneOfStr
import cats.parse.Parser.Expectation.EndOfString
import cats.parse.Parser.Expectation.WithContext

trait SourceParser[Alg[_[_]]] {
  def parse(s: String): Either[ParsingFailure, Alg[WithSource]]
  def map[Alg2[_[_]]](f: Alg[WithSource] => Alg2[WithSource]): SourceParser[Alg2] = parse(_).map(f)
}

object SourceParser {

  def apply[Alg[_[_]]](implicit F: SourceParser[Alg]): SourceParser[Alg] = F

  def fromCatsParseParser[Alg[_[_]]](
    parser: Parser0[Alg[WithSource]]
  ): SourceParser[Alg] =
    s =>
      parser
        .parseAll(s)
        .leftMap(ParsingFailure(_, s))

  implicit val useClauseParser: SourceParser[UseClause] = fromCatsParseParser(
    Parsers.parsers.useClause
  )

  implicit val preludeParser: SourceParser[Prelude] = fromCatsParseParser(
    Parsers.parsers.prelude
  )

  implicit val qonParser: SourceParser[QueryOperationName] = fromCatsParseParser(
    Parsers.parsers.queryOperationName
  )

  implicit val listedParser: SourceParser[Listed] = fromCatsParseParser(Parsers.parsers.listed)

  implicit val structParser: SourceParser[Struct] = fromCatsParseParser(Parsers.parsers.struct)

  implicit val queryParser: SourceParser[Query] = fromCatsParseParser(Parsers.parsers.query)

  implicit val sourceFileParser: SourceParser[SourceFile] = fromCatsParseParser(
    Parsers.parsers.sourceFile
  ).map { file =>
    // workaround: passing prelude's useClause to all queries
    file.copy[WithSource](
      statements =
        file
          .statements
          .nested
          .map { s =>
            s.fold(
              runQuery =
                rq =>
                  RunQuery[WithSource](
                    rq.query
                      .map(
                        _.copy(useClause =
                          file
                            .prelude
                            .useClause
                            .fold(WithSource.liftId(Option.empty[UseClause[WithSource]]))(
                              _.map(_.some)
                            )
                        )
                      )
                  )
            )
          }
          .value
    )
  }

}

case class ParsingFailure(underlying: Parser.Error, text: String) extends Exception {

  override def getMessage: String = msg

  private def showExpectation(verbose: Boolean, e: Parser.Expectation): String =
    e match {
      case OneOfStr(_, List(str))             => prep(str)
      case OneOfStr(_, strs)                  => strs.map(prep).mkString_(" OR ")
      case InRange(_, 'A', 'Z')               => "an uppercase letter"
      case InRange(_, 'a', 'z')               => "a lowercase letter"
      case InRange(_, '0', '9')               => "digit"
      case InRange(_, from, to) if from == to => prep(from.toString)
      case InRange(_, from, to) => s"one of ${prep(from.toString)} - ${prep(to.toString)}"
      case EndOfString(_, _)    => "end of string"
      case WithContext(contextStr, underlying) if verbose =>
        s"in ${Console.MAGENTA}$contextStr${Console.RESET}: ${showExpectation(verbose, underlying)}"
      case WithContext(_, underlying) => showExpectation(verbose, underlying)
      case e                          => e.toString
    }

  def expectationString(verbose: Boolean): String = underlying
    .expected
    .map(showExpectation(verbose, _))
    .mkString_(" OR ")

  private def prep(s: String): String = s.replace(' ', '·').replace("\n", "⏎\n")

  private def messageInternal(verbose: Boolean): String = {
    val (valid, failed) = text.splitAt(
      underlying.failedAtOffset
    )

    s"${Console.GREEN}${prep(valid)}${Console.RESET}${Console.YELLOW}${prep(failed)}${Console.RESET} - expected ${expectationString(verbose)} at offset ${underlying.failedAtOffset}"
  }

  def msg: String = messageInternal(verbose = false)
  def debug: String = messageInternal(verbose = true)

}

object ParsingFailure {
  implicit val eq: Eq[ParsingFailure] = Eq.fromUniversalEquals
}
