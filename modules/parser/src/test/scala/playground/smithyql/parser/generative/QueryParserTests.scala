package playground.smithyql.parser.generative

import playground.smithyql.*
import playground.smithyql.Diffs.given
import playground.smithyql.parser.Codecs.given
import playground.smithyql.parser.ParserSuite

object QueryParserTests extends ParserSuite {
  loadParserTests[Query]("query", trimWhitespace = true)
}
