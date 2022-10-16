package playground.smithyql.parser

import playground.smithyql._

import Diffs._
import Codecs._

object QueryParserTests extends ParserSuite {
  loadParserTests[Query]("query")
}
