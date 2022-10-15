package playground.smithyql

import com.softwaremill.diffx.Diff

object Diffs {
  import com.softwaremill.diffx.generic.auto._

  implicit val diffSourceRange: Diff[SourceRange] = Diff.derivedDiff
  implicit val diffQueryWithSource: Diff[Query[WithSource]] = Diff.derivedDiff
}