package playground.smithyql.parser

import cats.parse.Parser
import com.softwaremill.diffx.Diff

object Diffs {
  given Diff[Parser.Error] = Diff.useEquals
  given Diff[ParsingFailure] = Diff.derived
}
