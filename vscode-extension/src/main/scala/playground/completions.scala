package playground

import playground.smithyql.SmithyQLParser
import typings.vscode.mod

object completions {

  def complete(doc: mod.TextDocument, pos: mod.Position): List[mod.CompletionItem] =
    SmithyQLParser.parse(doc.getText()) match {
      case Left(_) =>
        // we can try to deal with this later
        Nil
      case Right(q) =>
        // todo
        val _ = q
        val _ = pos
        Nil
    }

}
