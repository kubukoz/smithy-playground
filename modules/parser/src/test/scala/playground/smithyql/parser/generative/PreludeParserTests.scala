package playground.smithyql.parser.generative

import playground.smithyql.Diffs.given
import playground.smithyql._
import playground.smithyql.parser.Codecs.given
import playground.smithyql.parser.ParserSuite

object PreludeParserTests extends ParserSuite {
  loadParserTests[Prelude]("prelude")
}
