package playground

import playground.Formatter

import playground.SmithyQLParser
import typings.vscode.mod
import typings.vscode.mod.TextDocument
import typings.vscode.mod.TextEdit
import scala.scalajs.js

object format {

  def perform(doc: TextDocument): js.Array[TextEdit] = {
    val firstLine = doc.lineAt(0)
    val lastLine = doc.lineAt(doc.lineCount - 1)

    scalajs
      .js
      .Array(
        TextEdit.replace(
          new mod.Range(
            firstLine.range.start,
            lastLine.range.end,
          ),
          Formatter.format(SmithyQLParser.parse(doc.getText()), 40),
        )
      )
  }

}
