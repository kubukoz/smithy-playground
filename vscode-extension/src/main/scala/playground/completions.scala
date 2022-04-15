package playground

import playground.smithyql.CompletionItem
import playground.smithyql.CompletionItemKind
import playground.smithyql.CompletionItemKind.EnumMember
import playground.smithyql.CompletionItemKind.Field
import playground.smithyql.CompletionItemKind.UnionMember
import smithy4s.Service
import typings.vscode.mod

import scala.scalajs.js.JSConverters._

import util.chaining._
import scalajs.js.|

object completions {

  def complete[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
    service: Service[Alg, Op]
  ): (mod.TextDocument, mod.Position) => List[mod.CompletionItem] = {
    val provider = CompletionProvider.make(service)

    (doc, pos) =>
      provider(
        doc.getText(),
        adapters.fromVscodePosition(doc)(pos),
      ).map(convertCompletion)
  }

  private def convertCompletion(item: CompletionItem): mod.CompletionItem = {
    val convertKind: CompletionItemKind => mod.CompletionItemKind = {
      case EnumMember                  => mod.CompletionItemKind.EnumMember
      case Field                       => mod.CompletionItemKind.Field
      case UnionMember                 => mod.CompletionItemKind.Class
      case CompletionItemKind.Function => mod.CompletionItemKind.Function
    }

    // todo determine RHS based on field type
    val insertText: String | mod.SnippetString =
      if (item.kind == CompletionItemKind.UnionMember)
        new mod.SnippetString(s"${item.label} = {$$0},")
      else
        s"${item.label} = "

    val docs: scalajs.js.UndefOr[mod.MarkdownString] =
      item
        .docs
        .fold[scalajs.js.UndefOr[mod.MarkdownString]](scalajs.js.undefined)(
          new mod.MarkdownString(_)
        )

    new mod.CompletionItem(item.label, convertKind(item.kind))
      .tap(_.insertText = insertText)
      .tap(_.detail = item.tpe)
      .tap(result =>
        if (item.deprecated)
          result.tags =
            List[mod.CompletionItemTag](
              mod.CompletionItemTag.Deprecated
            ).toJSArray
        else
          Nil
      )
      .tap(_.documentation = docs)
  }

}
