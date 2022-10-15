package playground.language

import com.softwaremill.diffx.Diff

object Diffs {
  import com.softwaremill.diffx.generic.auto._
  implicit val diffCompletionItem: Diff[CompletionItem] = Diff.derivedDiff
  implicit lazy val diffDocumentSymbol: Diff[DocumentSymbol] = Diff.derivedDiff
}
