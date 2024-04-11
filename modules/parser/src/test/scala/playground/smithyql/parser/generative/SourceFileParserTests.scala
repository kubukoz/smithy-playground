package playground.smithyql.parser.generative

import playground.smithyql.*
import playground.smithyql.Diffs.*
import playground.smithyql.parser.Codecs.*
import playground.smithyql.parser.ParserSuite

object SourceFileParserTests extends ParserSuite {
  loadParserTests[SourceFile]("source-file")
}
