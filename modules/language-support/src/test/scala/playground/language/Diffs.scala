package playground.language

import com.softwaremill.diffx.Diff

object Diffs {
  import com.softwaremill.diffx.generic.auto.*
  implicit val diffCompletionItem: Diff[CompletionItem] = Diff.derivedDiff
  implicit lazy val diffDocumentSymbol: Diff[DocumentSymbol] = Diff.derivedDiff
  implicit lazy val diffCodeLens: Diff[CodeLens] = Diff.derivedDiff
}
