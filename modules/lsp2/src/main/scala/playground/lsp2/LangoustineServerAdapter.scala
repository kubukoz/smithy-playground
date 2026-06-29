package playground.lsp2

import cats.Applicative
import cats.ApplicativeThrow
import cats.kernel.Semigroup
import cats.parse.LocationMap
import cats.syntax.all.*
import io.circe.Json
import langoustine.lsp.LSPBuilder
import langoustine.lsp.aliases.DocumentDiagnosticReport
import langoustine.lsp.aliases.TextDocumentContentChangeEvent
import langoustine.lsp.enumerations.CompletionItemKind
import langoustine.lsp.enumerations.InsertTextFormat
import langoustine.lsp.enumerations.MarkupKind
import langoustine.lsp.enumerations.TextDocumentSyncKind
import langoustine.lsp.requests.exit
import langoustine.lsp.requests.initialize
import langoustine.lsp.requests.shutdown
import langoustine.lsp.requests.textDocument
import langoustine.lsp.requests.workspace
import langoustine.lsp.runtime.Opt
import langoustine.lsp.structures.CodeLens
import langoustine.lsp.structures.CodeLensOptions
import langoustine.lsp.structures.CompletionItem
import langoustine.lsp.structures.CompletionOptions
import langoustine.lsp.structures.Diagnostic
import langoustine.lsp.structures.DiagnosticOptions
import langoustine.lsp.structures.DocumentSymbol
import langoustine.lsp.structures.InitializeResult
import langoustine.lsp.structures.InitializeResult.ServerInfo
import langoustine.lsp.structures.MarkupContent
import langoustine.lsp.structures.Position
import langoustine.lsp.structures.RelatedFullDocumentDiagnosticReport
import langoustine.lsp.structures.ServerCapabilities
import langoustine.lsp.structures.TextEdit
import playground.CompilationError
import playground.language.InsertText
import playground.language.Uri
import playground.lsp.LSPCodeLens
import playground.lsp.LSPCompletionItem
import playground.lsp.LSPDiagnostic
import playground.lsp.LSPDocumentSymbol
import playground.lsp.LSPPosition
import playground.lsp.LSPRange
import playground.lsp.LSPTextEdit
import playground.lsp.RunFileParams
import playground.lsp.ServerCapabilitiesCompiler
import playground.lsp2.ProtocolExtensions.smithyql
import playground.smithyql.SourceRange

object LangoustineServerAdapter {

  def adapt[F[_]: ApplicativeThrow](server: playground.lsp.LanguageServer[F])
    : LSPBuilder[F] => LSPBuilder[F] =
    _.handleRequest(initialize) { req =>
      server
        .initialize(
          req.params.workspaceFolders.toOption.foldMap(_.toOption.orEmpty).toList.map {
            workspaceFolder =>
              playground.language.Uri.fromUriString(workspaceFolder.uri.value)
          }
        )
        .map { result =>
          InitializeResult(
            capabilities = result
              .serverCapabilities(new ServerCapabilitiesCompiler {
                type Result = ServerCapabilities => ServerCapabilities

                def semigroup: Semigroup[ServerCapabilities => ServerCapabilities] = _.andThen(_)

                def codeLensProvider: Result = _.copy(codeLensProvider = Opt(CodeLensOptions()))

                def completionProvider: Result =
                  _.copy(completionProvider = Opt(CompletionOptions()))

                def diagnosticProvider: Result =
                  _.copy(diagnosticProvider =
                    Opt(
                      DiagnosticOptions(
                        interFileDependencies = false,
                        workspaceDiagnostics = false,
                      )
                    )
                  )

                def documentFormattingProvider: Result =
                  _.copy(documentFormattingProvider = Opt(true))

                def documentSymbolProvider: Result = _.copy(documentSymbolProvider = Opt(true))

                def textDocumentSync(kind: playground.lsp.TextDocumentSyncKind): Result =
                  _.copy(textDocumentSync = Opt(kind match {
                    case playground.lsp.TextDocumentSyncKind.Full => TextDocumentSyncKind.Full
                  }))

              })
              .apply(ServerCapabilities()),
            serverInfo = Opt(
              ServerInfo(
                name = result.serverInfo.name,
                version = Opt(result.serverInfo.version),
              )
            ),
          )
        }
    }
      // todo: are notifications handled fire-and-forget? or do we need a dispatcher?
      .handleNotification(textDocument.didChange) { req =>
        req
          .params
          .contentChanges
          .headOption
          .traverse_ { change =>
            change match {
              case _: TextDocumentContentChangeEvent.S0 =>
                new Exception("Unexpected incremental text change event").raiseError

              case TextDocumentContentChangeEvent.S1(newText) =>
                server.didChange(
                  documentUri = converters.fromLSP.uri(req.params.textDocument.uri),
                  newText = newText,
                )
            }
          }
      }
      .handleNotification(textDocument.didOpen) { req =>
        server.didOpen(
          documentUri = converters.fromLSP.uri(req.params.textDocument.uri),
          text = req.params.textDocument.text,
        )
      }
      .handleNotification(textDocument.didSave) { req =>
        server.didSave(converters.fromLSP.uri(req.params.textDocument.uri))
      }
      .handleNotification(textDocument.didClose) { req =>
        server.didClose(converters.fromLSP.uri(req.params.textDocument.uri))
      }
      .handleRequest(textDocument.formatting) { req =>
        server
          .formatting(documentUri = converters.fromLSP.uri(req.params.textDocument.uri))
          .map(_.map(converters.toLSP.textEdit))
          .map(edits => Opt(edits.toVector))
      }
      .handleRequest(textDocument.completion) { req =>
        server
          .completion(
            documentUri = converters.fromLSP.uri(req.params.textDocument.uri),
            position = converters.fromLSP.position(req.params.position),
          )
          .map(_.map(converters.toLSP.completionItem))
          .map(items => Opt(items.toVector))
      }
      .handleRequest(textDocument.diagnostic) { req =>
        server
          .diagnostic(documentUri = converters.fromLSP.uri(req.params.textDocument.uri))
          .map(_.map(converters.toLSP.diagnostic))
          .map(diags =>
            DocumentDiagnosticReport(
              RelatedFullDocumentDiagnosticReport(
                kind = "full",
                items = diags.toVector,
              )
            )
          )
      }
      .handleRequest(textDocument.codeLens) { req =>
        server
          .codeLens(documentUri = converters.fromLSP.uri(req.params.textDocument.uri))
          .map(_.map(converters.toLSP.codeLens))
          .map(edits => Opt(edits.toVector))
      }
      .handleRequest(workspace.executeCommand) { req =>
        server
          .executeCommand(
            commandName = req.params.command,
            arguments = req.params.arguments.toOption.orEmpty.toList.map(converters.fromLSP.json),
          )
          .as(Opt.empty)
      }
      .handleNotification(workspace.didChangeWatchedFiles)(_ => server.didChangeWatchedFiles)
      .handleRequest(textDocument.documentSymbol) { req =>
        server
          .documentSymbol(documentUri = converters.fromLSP.uri(req.params.textDocument.uri))
          .map(_.map(converters.toLSP.documentSymbol))
          .map(symbols => Opt(symbols.toVector))
      }
      .handleRequest(smithyql.runQuery) { req =>
        server.runFile(RunFileParams(converters.fromLSP.uri(req.params.uri)))
      }
      .handleNotification(exit) { _ =>
        System.err.println("we're in an exit now")
        Applicative[F].unit
      }
      .handleRequest(shutdown) { _ =>
        System.err.println("we're in a shutdown now")
        Applicative[F].pure(null: shutdown.Out /* Anton wtf */ )
      }

  object converters {

    object fromLSP {
      def uri(uri: langoustine.lsp.runtime.DocumentUri)
        : Uri = playground.language.Uri.fromUriString(uri.value)

      def json(u: ujson.Value): Json =
        u match {
          case ujson.Null     => Json.Null
          case ujson.True     => Json.True
          case ujson.False    => Json.False
          case ujson.Num(n)   => Json.fromDoubleOrNull(n)
          case ujson.Str(s)   => Json.fromString(s)
          case ujson.Arr(arr) => Json.fromValues(arr.map(json))
          case ujson.Obj(obj) => Json.fromFields(obj.map { case (k, v) => k -> json(v) })
        }

      def position(pos: Position): LSPPosition = LSPPosition(
        line = pos.line.value,
        character = pos.character.value,
      )

    }

    object toLSP {

      def completionItem(item: LSPCompletionItem): CompletionItem = completionItem(
        item.item,
        item.map,
      )

      private def completionItem(item: playground.language.CompletionItem, map: LocationMap)
        : CompletionItem = {
        val convertKind: playground.language.CompletionItemKind => CompletionItemKind = {
          case playground.language.CompletionItemKind.EnumMember  => CompletionItemKind.EnumMember
          case playground.language.CompletionItemKind.Field       => CompletionItemKind.Field
          case playground.language.CompletionItemKind.Constant    => CompletionItemKind.Constant
          case playground.language.CompletionItemKind.UnionMember => CompletionItemKind.Class
          case playground.language.CompletionItemKind.Function    => CompletionItemKind.Function
          case playground.language.CompletionItemKind.Module      => CompletionItemKind.Module
        }

        val insertText =
          item.insertText match {
            case InsertText.JustString(value)    => value
            case InsertText.SnippetString(value) => value
          }

        val insertTextFmt =
          item.insertText match {
            case _: InsertText.JustString    => InsertTextFormat.PlainText
            case _: InsertText.SnippetString => InsertTextFormat.Snippet
          }

        val additionalTextEdits: List[TextEdit] = item
          .extraTextEdits
          .map(textEdit(_, map))

        CompletionItem(
          label = item.label,
          labelDetails = Opt(
            langoustine
              .lsp
              .structures
              .CompletionItemLabelDetails(
                detail = Opt(item.detail)
              )
          ),
          detail = Opt.fromOption(item.description),
          kind = Opt(convertKind(item.kind)),
          insertText = Opt(insertText),
          insertTextFormat = Opt(insertTextFmt),
          additionalTextEdits = Opt(additionalTextEdits.toVector),
          tags = Opt {
            if item.deprecated then Vector(
              langoustine.lsp.enumerations.CompletionItemTag.Deprecated
            )
            else
              Vector.empty
          },
          documentation = Opt.fromOption(item.docs.map { docs =>
            MarkupContent(
              kind = MarkupKind.Markdown,
              value = docs,
            )
          }),
          sortText = Opt.fromOption(item.sortText),
        )
      }

      def codeLens(lens: LSPCodeLens): CodeLens = codeLens(lens.lens, lens.map)

      def codeLens(lens: playground.language.CodeLens, map: LocationMap): CodeLens = CodeLens(
        range = range(LSPRange.from(lens.range, map)),
        command = Opt {
          langoustine
            .lsp
            .structures
            .Command(
              title = lens.command.title,
              command = lens.command.command,
              arguments = Opt(lens.command.args.map(ujson.Str(_)).toVector),
            )
        },
      )

      def textEdit(edit: LSPTextEdit): TextEdit = textEdit(edit.textEdit, edit.map)

      private def textEdit(edit: playground.language.TextEdit, map: LocationMap): TextEdit =
        edit match {
          case playground.language.TextEdit.Insert(what, where) =>
            TextEdit(
              range = toLSP.range(LSPRange.from(SourceRange(where, where), map)),
              newText = what,
            )

          case playground.language.TextEdit.Overwrite(what, range) =>
            TextEdit(
              range = toLSP.range(LSPRange.from(range, map)),
              newText = what,
            )
        }

      def diagnostic(diag: LSPDiagnostic): Diagnostic = diagnostic(diag.diagnostic, diag.map)

      private def diagnostic(diag: CompilationError, map: LocationMap): Diagnostic = Diagnostic(
        range = toLSP.range(LSPRange.from(diag.range, map)),
        message = diag.err.render,
        severity = Opt {
          diag.severity match {
            case playground.DiagnosticSeverity.Error =>
              langoustine.lsp.enumerations.DiagnosticSeverity.Error
            case playground.DiagnosticSeverity.Information =>
              langoustine.lsp.enumerations.DiagnosticSeverity.Information
            case playground.DiagnosticSeverity.Warning =>
              langoustine.lsp.enumerations.DiagnosticSeverity.Warning
          }
        },
        tags = Opt(
          diag
            .tags
            .map {
              case playground.DiagnosticTag.Deprecated =>
                langoustine.lsp.enumerations.DiagnosticTag.Deprecated
              case playground.DiagnosticTag.Unused =>
                langoustine.lsp.enumerations.DiagnosticTag.Unnecessary
            }
            .toVector
        ),
      )

      def documentSymbol(sym: LSPDocumentSymbol): DocumentSymbol = documentSymbol(sym.sym, sym.map)

      private def documentSymbol(sym: playground.language.DocumentSymbol, map: LocationMap)
        : DocumentSymbol = DocumentSymbol(
        name = sym.name,
        kind =
          sym.kind match {
            case playground.language.SymbolKind.Array =>
              langoustine.lsp.enumerations.SymbolKind.Array
            case playground.language.SymbolKind.Field =>
              langoustine.lsp.enumerations.SymbolKind.Field
            case playground.language.SymbolKind.Function =>
              langoustine.lsp.enumerations.SymbolKind.Function
            case playground.language.SymbolKind.Package =>
              langoustine.lsp.enumerations.SymbolKind.Package
          },
        range = converters
          .toLSP
          .range(
            LSPRange.from(sym.range, map)
          ),
        selectionRange = converters
          .toLSP
          .range(
            LSPRange.from(sym.selectionRange, map)
          ),
        children = Opt(sym.children.map(documentSymbol(_, map)).toVector),
      )

      def range(range: LSPRange): langoustine.lsp.structures.Range = langoustine
        .lsp
        .structures
        .Range(
          start = position(range.from),
          end = position(range.to),
        )

      def position(position: LSPPosition): langoustine.lsp.structures.Position = langoustine
        .lsp
        .structures
        .Position(
          line = position.line,
          character = position.character,
        )

    }

  }

}
