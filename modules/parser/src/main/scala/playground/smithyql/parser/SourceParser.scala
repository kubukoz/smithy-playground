package playground.smithyql.parser

import cats.implicits._
import cats.kernel.Eq
import cats.parse.Parser
import cats.parse.Parser.Expectation.InRange
import cats.parse.Parser0
import playground.smithyql._

trait SourceParser[Alg[_[_]]] {
  def parse(s: String): Either[ParsingFailure, Alg[WithSource]]
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

  implicit val listedParser: SourceParser[Listed] = fromCatsParseParser(Parsers.parsers.listed)

  implicit val structParser: SourceParser[Struct] = fromCatsParseParser(Parsers.parsers.struct)

  implicit val queryParser: SourceParser[Query] = fromCatsParseParser(Parsers.parsers.query)
}

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

    def prep(s: String): String = s.replace(' ', '·').replace("\n", "⏎\n")

    s"${Console.GREEN}${prep(valid)}${Console.RESET}${Console.YELLOW}${prep(failed)}${Console.RESET} - expected ${underlying
        .expected
        .map(showExpectation)
        .mkString_("/")} at offset ${underlying.failedAtOffset}"
  }

}

object ParsingFailure {
  implicit val eq: Eq[ParsingFailure] = Eq.fromUniversalEquals
}
