package playground

import playground.smithyql.SourceRange
import typings.vscode.mod

object adapters {

  def toVscodeRange(doc: mod.TextDocument, range: SourceRange): mod.Range = {
    val pos = doc.positionAt(range.start.index.toDouble)
    val end = doc.positionAt(range.end.index.toDouble)
    new mod.Range(pos, end)
  }

}
