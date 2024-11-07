package playground.smithyql.parser.generative

import playground.smithyql.*
import playground.smithyql.Diffs.given
import playground.smithyql.parser.Codecs.given
import playground.smithyql.parser.ParserSuite
import playground.smithyql.parser.SourceParser

object ListParserTests extends ParserSuite {

  override def treeSitterWrap(fileSource: String): String =
    s"""FakeCall {
       |  fakeField = $fileSource
       |}""".stripMargin

  loadParserTests[Listed]("listed", trimWhitespace = true)
}
