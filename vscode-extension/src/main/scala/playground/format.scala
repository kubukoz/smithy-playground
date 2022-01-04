package playground

import playground.Formatter
import playground.SmithyQLParser
import typings.vscode.mod
import typings.vscode.mod.TextDocument
import typings.vscode.mod.TextEdit

import scala.scalajs.js

object format {

  def perform(doc: TextDocument): js.Array[TextEdit] = {
    val maxWidth = mod
      .workspace
      .getConfiguration()
      .get[Int]("smithyql.formatter.maxWidth")
      .getOrElse(sys.error("no maxWidth set"))

    val firstLine = doc.lineAt(0)
    val lastLine = doc.lineAt(doc.lineCount - 1)

    SmithyQLParser
      .parse(doc.getText())
      .map { parsed =>
        scalajs
          .js
          .Array(
            TextEdit.replace(
              new mod.Range(
                firstLine.range.start,
                lastLine.range.end,
              ),
              Formatter.format(parsed, maxWidth),
            )
          )
      }
      .getOrElse(js.Array())
  }

}
