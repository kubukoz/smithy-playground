package playground

import playground.smithyql.CompletionItem
import playground.smithyql.CompletionItemKind
import playground.smithyql.CompletionItemKind.EnumMember
import playground.smithyql.CompletionItemKind.Field
import playground.smithyql.CompletionItemKind.UnionMember
import playground.smithyql.InsertText.JustString
import playground.smithyql.InsertText.SnippetString
import typings.vscode.mod

import scala.scalajs.js.JSConverters._

import util.chaining._
import scalajs.js.|

object completions {

  def complete[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
    provider: CompletionProvider
  ): (mod.TextDocument, mod.Position) => List[mod.CompletionItem] = { (doc, pos) =>
    provider
      .provide(
        doc.getText(),
        adapters.fromVscodePosition(doc)(pos),
      )
      .map(convertCompletion)
  }

  private def convertCompletion(item: CompletionItem): mod.CompletionItem = {
    val convertKind: CompletionItemKind => mod.CompletionItemKind = {
      case EnumMember                  => mod.CompletionItemKind.EnumMember
      case Field                       => mod.CompletionItemKind.Field
      case CompletionItemKind.Constant => mod.CompletionItemKind.Constant
      case UnionMember                 => mod.CompletionItemKind.Class
      case CompletionItemKind.Function => mod.CompletionItemKind.Function
    }

    val insertText: String | mod.SnippetString =
      item.insertText match {
        case JustString(value)    => value
        case SnippetString(value) => new mod.SnippetString(value)
      }

    val docs: scalajs.js.UndefOr[mod.MarkdownString] =
      item
        .docs
        .map(new mod.MarkdownString(_))
        .orUndefined

    new mod.CompletionItem(
      mod
        .CompletionItemLabel(item.label)
        .tap(_.detail = item.detail)
        .tap(_.description = item.description.orUndefined),
      convertKind(item.kind),
    )
      .tap(_.insertText = insertText)
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
