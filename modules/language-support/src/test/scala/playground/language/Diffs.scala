package playground.language

import com.softwaremill.diffx.Diff
import playground.smithyql.Diffs.given

object Diffs {
  given Diff[CompletionItemKind] = Diff.derived
  given Diff[TextEdit] = Diff.derived
  given Diff[InsertText] = Diff.derived
  given Diff[CompletionItem] = Diff.derived
  given Diff[SymbolKind] = Diff.derived
  given Diff[DocumentSymbol] = Diff.derived
  given Diff[Command] = Diff.derived
  given Diff[CodeLens] = Diff.derived
}
