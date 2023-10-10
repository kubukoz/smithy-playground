package playground.smithyql.parser.v2

import com.softwaremill.diffx.Diff
import playground.smithyql.parser.v2.scanner.Token
import playground.smithyql.parser.v2.scanner.TokenKind

object Diffs {

  implicit val tokenKindDiff: Diff[TokenKind] = Diff.derived
  implicit val tokenDiff: Diff[Token] = Diff.derived

}
