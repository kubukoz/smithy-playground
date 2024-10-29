package playground.smithyql.parser.generative

import playground.smithyql.*
import playground.smithyql.Diffs.given
import playground.smithyql.parser.Codecs.given
import playground.smithyql.parser.ParserSuite

object StructParserTests extends ParserSuite {
  loadParserTests[Struct]("struct", trimWhitespace = true)
}
