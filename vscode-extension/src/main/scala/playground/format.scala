package playground

import playground.smithyql.Formatter
import playground.smithyql.SmithyQLParser
import typings.vscode.mod
import typings.vscode.mod.TextDocument
import typings.vscode.mod.TextEdit

object format {

  def perform(doc: TextDocument): List[TextEdit] = {
    val maxWidth = mod
      .workspace
      .getConfiguration()
      .get[Int]("smithyql.formatter.maxWidth")
      .getOrElse(sys.error("no maxWidth set"))

    val firstLine = doc.lineAt(0)
    val lastLine = doc.lineAt(doc.lineCount - 1)

    SmithyQLParser
      .parseFull(doc.getText())
      .map { parsed =>
        List(
          TextEdit.replace(
            new mod.Range(
              firstLine.range.start,
              lastLine.range.end,
            ),
            Formatter.format(parsed, maxWidth),
          )
        )
      }
      .getOrElse(Nil)
  }

}
