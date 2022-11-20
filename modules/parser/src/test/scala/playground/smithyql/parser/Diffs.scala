package playground.smithyql.parser

import com.softwaremill.diffx.Diff

object Diffs {
  import com.softwaremill.diffx.generic.auto._

  implicit val diffParsingFailure: Diff[ParsingFailure] = Diff.derivedDiff
}
