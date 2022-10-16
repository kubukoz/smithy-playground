package playground.smithyql.parser

import playground.smithyql._

import Diffs._
import Codecs._

object ListParserTests extends ParserSuite {
  loadParserTests[Listed]("listed", trimWhitespace = true)
}
