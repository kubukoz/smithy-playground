package playground.smithyql.parser.generative.negative

import playground.smithyql.Prelude
import playground.smithyql.parser.ParserSuite

object PreludeParserNegativeTests extends ParserSuite {
  loadNegativeParserTests[Prelude]("prelude", trimWhitespace = true, invalidTokens = false)
}
