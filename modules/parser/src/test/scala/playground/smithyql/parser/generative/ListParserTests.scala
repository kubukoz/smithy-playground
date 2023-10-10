package playground.smithyql.parser.generative

import playground.smithyql.Diffs.given
import playground.smithyql._
import playground.smithyql.parser.Codecs.given
import playground.smithyql.parser.ParserSuite
import playground.smithyql.parser.SourceParser

object ListParserTests extends ParserSuite {
  loadParserTests[Listed]("listed", trimWhitespace = true)
}
