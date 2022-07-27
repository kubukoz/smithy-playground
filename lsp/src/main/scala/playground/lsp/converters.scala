package playground.lsp

import cats.parse.LocationMap
import org.eclipse.lsp4j
import playground.smithyql.CompletionItem
import playground.smithyql.CompletionItemKind
import playground.smithyql.InsertText
import playground.smithyql.Position
import playground.smithyql.TextEdit

import scala.jdk.CollectionConverters._
import scala.util.chaining._

object converters {

  object toLSP {

    def position(
      doc: String,
      pos: Position,
    ): lsp4j.Position = {
      val caret = LocationMap(doc).toCaretUnsafe(pos.index)
      new lsp4j.Position(caret.line, caret.col)
    }

    def completionItem(
      doc: String,
      item: CompletionItem,
    ): lsp4j.CompletionItem = {
      val convertKind: CompletionItemKind => lsp4j.CompletionItemKind = {
        case CompletionItemKind.EnumMember  => lsp4j.CompletionItemKind.EnumMember
        case CompletionItemKind.Field       => lsp4j.CompletionItemKind.Field
        case CompletionItemKind.Constant    => lsp4j.CompletionItemKind.Constant
        case CompletionItemKind.UnionMember => lsp4j.CompletionItemKind.Class
        case CompletionItemKind.Function    => lsp4j.CompletionItemKind.Function
      }

      val insertText =
        item.insertText match {
          case InsertText.JustString(value)    => value
          case InsertText.SnippetString(value) => value
        }

      val insertTextFmt =
        item.insertText match {
          case _: InsertText.JustString    => lsp4j.InsertTextFormat.PlainText
          case _: InsertText.SnippetString => lsp4j.InsertTextFormat.Snippet
        }

      val additionalTextEdits: List[lsp4j.TextEdit] = item.extraTextEdits.map {
        case TextEdit.Insert(what, where) =>
          val pos = converters.toLSP.position(doc, where)
          new lsp4j.TextEdit()
            .tap(_.setNewText(what))
            .tap(_.setRange(new lsp4j.Range(pos, pos)))
      }

      new lsp4j.CompletionItem(
        item.label
      )
        .tap(
          _.setLabelDetails(
            new lsp4j.CompletionItemLabelDetails()
              .tap(_.setDetail(item.detail))
          )
        )
        .tap(item.description match {
          case None       => identity
          case Some(desc) => _.setDetail(desc)
        })
        .tap(_.setKind(convertKind(item.kind)))
        .tap(_.setInsertText(insertText))
        .tap(_.setInsertTextFormat(insertTextFmt))
        .tap(_.setAdditionalTextEdits(additionalTextEdits.asJava))
        .tap(result =>
          result.setTags {
            {
              if (item.deprecated)
                List(lsp4j.CompletionItemTag.Deprecated)
              else
                Nil
            }.asJava
          }
        )
        .tap(item.docs match {
          case None => identity
          case Some(theDocs) =>
            _.setDocumentation(new lsp4j.MarkupContent(lsp4j.MarkupKind.MARKDOWN, theDocs))
        })
    }

  }

  object fromLSP {

    def position(
      doc: String,
      pos: lsp4j.Position,
    ): Position = Position(
      LocationMap(doc).toOffset(pos.getLine(), pos.getCharacter()).getOrElse(-1)
    )

  }

}
