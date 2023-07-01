package playground.smithyql.parser.generative

import playground.smithyql.Diffs._
import playground.smithyql._
import playground.smithyql.parser.Codecs._
import playground.smithyql.parser.ParserSuite

object ListParserTests extends ParserSuite {
  loadParserTests[Listed]("listed", trimWhitespace = true)
}
