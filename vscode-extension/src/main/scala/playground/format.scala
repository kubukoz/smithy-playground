package playground

import playground.smithyql.Formatter
import playground.smithyql.SmithyQLParser
import typings.vscode.mod
import typings.vscode.mod.TextDocument
import typings.vscode.mod.TextEdit

import debug.timed

object format {

  def perform(doc: TextDocument): List[TextEdit] = {
    val maxWidth = vscodeutil.unsafeGetConfig[Int]("smithyql.formatter.maxWidth")

    val firstLine = doc.lineAt(0)
    val lastLine = doc.lineAt(doc.lineCount - 1)
    timed("parsing") {
      SmithyQLParser
        .parseFull(doc.getText())
    }
      .map { parsed =>
        List(
          TextEdit.replace(
            new mod.Range(
              firstLine.range.start,
              lastLine.range.end,
            ),
            timed("formatting")(Formatter.format(parsed, maxWidth)),
          )
        )
      }
      .getOrElse(Nil)
  }

}
