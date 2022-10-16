package playground.smithyql.parser

import playground.smithyql._

import Diffs._
import Codecs._

object UseServiceParserTests extends ParserSuite {
  loadParserTests[UseClause]("use-clause", trimWhitespace = true)
}
