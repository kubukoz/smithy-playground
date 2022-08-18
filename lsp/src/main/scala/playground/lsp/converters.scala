package playground.lsp

import cats.implicits._
import cats.parse.LocationMap
import com.google.gson.JsonElement
import io.circe.Json
import io.circe.JsonNumber
import org.eclipse.lsp4j
import playground.CodeLens
import playground.CompilationError
import playground.DiagnosticSeverity
import playground.DiagnosticTag
import playground.DocumentSymbol
import playground.SymbolKind
import playground.smithyql.CompletionItem
import playground.smithyql.CompletionItemKind
import playground.smithyql.InsertText
import playground.smithyql.Position
import playground.smithyql.SourceRange
import playground.smithyql.TextEdit

import scala.jdk.CollectionConverters._
import scala.util.chaining._
import playground.DocumentReference.SameFile
import playground.RelativeLocation

object converters {

  object toLSP {

    def documentSymbol(docText: String, sym: DocumentSymbol): lsp4j.DocumentSymbol =
      new lsp4j.DocumentSymbol(
        sym.name,
        symbolKind(sym.kind),
        range(docText, sym.range),
        range(docText, sym.range),
      ).tap(_.setChildren(sym.children.map(documentSymbol(docText, _)).asJava))

    def symbolKind(kind: SymbolKind): lsp4j.SymbolKind =
      kind match {
        case SymbolKind.Function => lsp4j.SymbolKind.Function
        case SymbolKind.Array    => lsp4j.SymbolKind.Array
        case SymbolKind.Field    => lsp4j.SymbolKind.Field
        case SymbolKind.Package  => lsp4j.SymbolKind.Package
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
        case CompletionItemKind.Module      => lsp4j.CompletionItemKind.Module
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

    def diagnostic(doc: String, documentUri: String, diag: CompilationError): lsp4j.Diagnostic =
      new lsp4j.Diagnostic()
        .tap(_.setRange(toLSP.range(doc, diag.range)))
        .tap(_.setMessage(diag.err.render))
        .tap(_.setSeverity(diag.severity match {
          case DiagnosticSeverity.Error       => lsp4j.DiagnosticSeverity.Error
          case DiagnosticSeverity.Information => lsp4j.DiagnosticSeverity.Information
          case DiagnosticSeverity.Warning     => lsp4j.DiagnosticSeverity.Warning
        }))
        .tap(
          _.setRelatedInformation(
            diag
              .relatedInfo
              .map { info =>
                new lsp4j.DiagnosticRelatedInformation(
                  location(doc, documentUri, info.location),
                  info.message.render,
                )
              }
              .asJava
          )
        )
        .tap(
          _.setTags(
            diag
              .tags
              .map { tag =>
                tag match {
                  case DiagnosticTag.Deprecated => lsp4j.DiagnosticTag.Deprecated
                  case DiagnosticTag.Unused     => lsp4j.DiagnosticTag.Unnecessary
                }
              }
              .toList
              .asJava
          )
        )

    private def location(doc: String, documentUri: String, loc: RelativeLocation): lsp4j.Location =
      new lsp4j.Location(
        loc.document match {
          case SameFile => documentUri
        },
        range(doc, loc.range),
      )

    def codeLens(documentText: String, lens: CodeLens): lsp4j.CodeLens =
      new lsp4j.CodeLens(range(documentText, lens.range))
        .tap(
          _.setCommand(
            new lsp4j.Command()
              .tap(_.setTitle(lens.command.title))
              .tap(_.setCommand(lens.command.command))
              .tap(_.setArguments(lens.command.args.widen[Object].asJava))
          )
        )

    def range(
      doc: String,
      coreRange: SourceRange,
    ): lsp4j.Range = new lsp4j.Range(position(doc, coreRange.start), position(doc, coreRange.end))

    def position(
      doc: String,
      pos: Position,
    ): lsp4j.Position = {
      val caret = LocationMap(doc).toCaretUnsafe(pos.index)
      new lsp4j.Position(caret.line, caret.col)
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

  def gsonToCirce(gson: JsonElement): Json =
    if (gson.isJsonPrimitive()) {
      val prim = gson.getAsJsonPrimitive()

      if (prim.isString())
        Json.fromString(prim.getAsString())
      else if (prim.isNumber())
        Json.fromJsonNumber(JsonNumber.fromString(prim.getAsString()).get)
      else if (prim.isBoolean())
        Json.fromBoolean(prim.getAsBoolean())
      else
        throw new IllegalArgumentException(s"Unknown primitive: $prim")
    } else if (gson.isJsonArray()) {
      Json.fromValues(gson.getAsJsonArray().asScala.map(gsonToCirce).toList)
    } else if (gson.isJsonObject()) {
      Json.fromFields(
        gson
          .getAsJsonObject()
          .entrySet()
          .asScala
          .map { case entry =>
            val key = entry.getKey
            val value = gsonToCirce(entry.getValue)
            key -> value
          }
          .toList
      )
    } else
      Json.Null

}
