package playground.smithyql.parser.generative

import playground.smithyql.Diffs._
import playground.smithyql._
import playground.smithyql.parser.Codecs._
import playground.smithyql.parser.ParserSuite

object StructParserTests extends ParserSuite {
  loadParserTests[Struct]("struct", trimWhitespace = true)
}
