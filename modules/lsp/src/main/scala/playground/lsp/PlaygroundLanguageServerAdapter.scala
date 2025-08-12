package playground.lsp

import cats.effect.kernel.Sync
import cats.effect.std.Dispatcher
import cats.kernel.Semigroup
import cats.syntax.all.*
import com.google.gson.JsonElement
import org.eclipse.lsp4j
import org.eclipse.lsp4j.adapters.DocumentSymbolResponseAdapter
import org.eclipse.lsp4j.jsonrpc.json.ResponseJsonAdapter
import org.eclipse.lsp4j.jsonrpc.messages
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest
import util.chaining.*

import java.util.concurrent.CompletableFuture
import scala.annotation.nowarn
import scala.jdk.CollectionConverters.*

final class PlaygroundLanguageServerAdapter[F[_]: Sync](
  impl: LanguageServer[F]
)(
  implicit d: Dispatcher[F]
) {

  // no backpressure whatsoever for now
  private def handleNotification(
    job: F[Unit]
  ): Unit = d.unsafeRunAndForget(job)

  @JsonRequest("initialize")
  def initialize(
    params: lsp4j.InitializeParams
  ): CompletableFuture[lsp4j.InitializeResult] = d.unsafeToCompletableFuture(
    impl
      .initialize(
        params
          .getWorkspaceFolders()
          .asScala
          .map(converters.fromLSP.uri(_))
          .toList
      )
      .map { result =>
        new lsp4j.InitializeResult(
          new lsp4j.ServerCapabilities().tap(
            result.serverCapabilities(
              new ServerCapabilitiesCompiler {
                type Result = lsp4j.ServerCapabilities => Unit

                val semigroup: Semigroup[Result] =
                  (f1, f2) =>
                    sc => {
                      f1(sc)
                      f2(sc)
                    }

                def textDocumentSync(kind: TextDocumentSyncKind): Result =
                  _.setTextDocumentSync(converters.toLSP.textDocumentSyncKind(kind))

                def documentFormattingProvider: Result = _.setDocumentFormattingProvider(true)

                def completionProvider: Result =
                  _.setCompletionProvider(new lsp4j.CompletionOptions())

                def diagnosticProvider: Result =
                  _.setDiagnosticProvider(new lsp4j.DiagnosticRegistrationOptions())

                def codeLensProvider: Result = _.setCodeLensProvider(new lsp4j.CodeLensOptions())

                def documentSymbolProvider: Result = _.setDocumentSymbolProvider(true)

                def definitionProvider: Result = _.setDefinitionProvider(true)
              }
            )
          ),
          converters.toLSP.serverInfo(result.serverInfo),
        )
      }
  )

  @JsonNotification("initialized")
  def initialize(
    params: lsp4j.InitializedParams
  ): Unit = ()

  @JsonRequest("shutdown")
  def shutdown(
  ): CompletableFuture[Object] = CompletableFuture.completedFuture(null)

  @JsonNotification("textDocument/didChange")
  def didChange(
    params: lsp4j.DidChangeTextDocumentParams
  ): Unit =
    if (params.getContentChanges().isEmpty())
      ()
    else
      handleNotification(
        impl.didChange(
          documentUri = converters.fromLSP.uri(params.getTextDocument),
          newText = params.getContentChanges.asScala.head.getText(),
        )
      )

  @JsonNotification("textDocument/didOpen")
  def didOpen(
    params: lsp4j.DidOpenTextDocumentParams
  ): Unit = handleNotification(
    impl.didOpen(
      documentUri = converters.fromLSP.uri(params.getTextDocument),
      text = params.getTextDocument.getText,
    )
  )

  @JsonNotification("textDocument/didSave")
  def didSave(
    params: lsp4j.DidSaveTextDocumentParams
  ): Unit = handleNotification(
    impl.didSave(
      documentUri = converters.fromLSP.uri(params.getTextDocument)
    )
  )

  @JsonNotification("textDocument/didClose")
  def didClose(
    params: lsp4j.DidCloseTextDocumentParams
  ): Unit = handleNotification(
    impl.didClose(
      documentUri = converters.fromLSP.uri(params.getTextDocument)
    )
  )

  @JsonRequest("textDocument/formatting")
  def formatting(
    params: lsp4j.DocumentFormattingParams
  ): CompletableFuture[java.util.List[lsp4j.TextEdit]] = d.unsafeToCompletableFuture(
    impl
      .formatting(
        documentUri = converters.fromLSP.uri(params.getTextDocument)
      )
      .map { edits =>
        edits.map(converters.toLSP.textEdit).asJava
      }
  )

  @JsonRequest("textDocument/completion")
  def completion(
    params: lsp4j.CompletionParams
  ): CompletableFuture[
    messages.Either[java.util.List[lsp4j.CompletionItem], lsp4j.CompletionList]
  ] = d
    .unsafeToCompletableFuture(
      impl
        .completion(
          documentUri = converters.fromLSP.uri(params.getTextDocument),
          position = converters.fromLSP.position(params.getPosition),
        )
        .map {
          _.map(converters.toLSP.completionItem)
            .asJava
            .pipe(messages.Either.forLeft(_))
        }
    )

  @JsonRequest("textDocument/definition")
  def definition(
    params: lsp4j.DefinitionParams
  ): CompletableFuture[
    messages.Either[java.util.List[lsp4j.Location], java.util.List[lsp4j.LocationLink]]
  ] = d
    .unsafeToCompletableFuture {
      params.getTextDocument()
      params.getPosition()

      impl
        .definition(
          documentUri = converters.fromLSP.uri(params.getTextDocument),
          position = converters.fromLSP.position(params.getPosition),
        )
        .map { locations =>
          locations.map(converters.toLSP.location).asJava.pipe(messages.Either.forLeft(_))
        }
    }

  @JsonRequest("textDocument/diagnostic")
  def diagnostic(
    params: lsp4j.DocumentDiagnosticParams
  ): CompletableFuture[lsp4j.DocumentDiagnosticReport] = d.unsafeToCompletableFuture(
    impl
      .diagnostic(
        documentUri = converters.fromLSP.uri(params.getTextDocument)
      )
      .map { diags =>
        new lsp4j.DocumentDiagnosticReport(
          new lsp4j.RelatedFullDocumentDiagnosticReport(
            diags.map(converters.toLSP.diagnostic).asJava
          )
        )
      }
  )

  @JsonRequest("textDocument/codeLens")
  def codeLens(
    params: lsp4j.CodeLensParams
  ): CompletableFuture[java.util.List[lsp4j.CodeLens]] = d.unsafeToCompletableFuture(
    impl
      .codeLens(
        documentUri = converters.fromLSP.uri(params.getTextDocument)
      )
      .map(_.map(converters.toLSP.codeLens).asJava)
  )

  @JsonRequest("workspace/executeCommand")
  def executeCommand(
    params: lsp4j.ExecuteCommandParams
  ): CompletableFuture[Object] = d.unsafeToCompletableFuture(
    Sync[F]
      .defer {
        impl
          .executeCommand(
            commandName = params.getCommand(),
            arguments = params
              .getArguments()
              .asScala
              .toList
              .map {
                case gson: JsonElement => converters.gsonToCirce(gson)
                case s                 => throw new Throwable("Unsupported arg: " + s)
              },
          )
      }
      .as(null: Object)
  )

  @JsonNotification("workspace/didChangeWatchedFiles")
  def didChangeWatchedFiles(
    @nowarn("msg=unused")
    params: lsp4j.DidChangeWatchedFilesParams
  ): Unit = handleNotification(impl.didChangeWatchedFiles)

  @JsonRequest("textDocument/documentSymbol")
  @ResponseJsonAdapter(classOf[DocumentSymbolResponseAdapter])
  def documentSymbol(
    params: lsp4j.DocumentSymbolParams
  ): CompletableFuture[java.util.List[messages.Either[Nothing, lsp4j.DocumentSymbol]]] = d
    .unsafeToCompletableFuture(
      impl
        .documentSymbol(converters.fromLSP.uri(params.getTextDocument()))
        .map(_.map(converters.toLSP.documentSymbol).map(messages.Either.forRight(_)).asJava)
    )

  @JsonRequest("smithyql/runQuery")
  def runQuery(
    params: RunFileParams
  ): CompletableFuture[Object] = d.unsafeToCompletableFuture(impl.runFile(params).as(null: Object))

  @JsonRequest("exit")
  def exit(
  ): CompletableFuture[Object] = CompletableFuture.completedFuture(null)

}
