package playground.smithyql.parser

import playground.smithyql._

import Diffs._
import Codecs._

object StructParserTests extends ParserSuite {
  loadParserTests[Struct]("struct", trimWhitespace = true)
}
