package playground.smithyql.parser.generative

import playground.smithyql.*
import playground.smithyql.Diffs.given
import playground.smithyql.parser.Codecs.given
import playground.smithyql.parser.ParserSuite

object StructParserTests extends ParserSuite {

  override def treeSitterWrap(fileSource: String): String = s"FakeCall $fileSource"
  loadParserTests[Struct]("struct", trimWhitespace = true)
}
