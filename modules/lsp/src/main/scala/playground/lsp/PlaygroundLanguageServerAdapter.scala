package playground.lsp

import cats.Functor
import cats.effect.std.Dispatcher
import cats.implicits._
import com.google.gson.JsonPrimitive
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.adapters.DocumentSymbolResponseAdapter
import org.eclipse.lsp4j.jsonrpc.json.ResponseJsonAdapter
import org.eclipse.lsp4j.jsonrpc.messages
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest

import java.util.concurrent.CompletableFuture
import scala.jdk.CollectionConverters._
import scala.util.chaining._

final class PlaygroundLanguageServerAdapter[F[_]: Functor](
  impl: LanguageServer[F]
)(
  implicit d: Dispatcher[F]
) {

  @JsonRequest("initialize")
  def initialize(
    params: InitializeParams
  ): CompletableFuture[InitializeResult] = d.unsafeToCompletableFuture(impl.initialize(params))

  @JsonNotification("initialized")
  def initialize(
    params: InitializedParams
  ): Unit = d.unsafeRunSync(impl.initialized(params))

  @JsonRequest("shutdown")
  def shutdown(
  ): CompletableFuture[Object] = d.unsafeToCompletableFuture(impl.shutdown.as(null: Object))

  @JsonNotification("textDocument/didChange")
  def didChange(
    params: DidChangeTextDocumentParams
  ): Unit = d.unsafeRunSync(impl.didChange(params))

  @JsonNotification("textDocument/didOpen")
  def didOpen(
    params: DidOpenTextDocumentParams
  ): Unit = d.unsafeRunSync(impl.didOpen(params))

  @JsonNotification("textDocument/didSave")
  def didSave(
    params: DidSaveTextDocumentParams
  ): Unit = d.unsafeRunSync(impl.didSave(params))

  @JsonNotification("textDocument/didClose")
  def didClose(
    params: DidCloseTextDocumentParams
  ): Unit = d.unsafeRunSync(impl.didClose(params))

  @JsonRequest("textDocument/formatting")
  def formatting(
    params: DocumentFormattingParams
  ): CompletableFuture[java.util.List[TextEdit]] = d.unsafeToCompletableFuture(
    impl.formatting(params).map(_.asJava)
  )

  @JsonRequest("textDocument/completion")
  def completion(
    params: CompletionParams
  ): CompletableFuture[messages.Either[java.util.List[CompletionItem], CompletionList]] = d
    .unsafeToCompletableFuture(
      impl
        .completion(params)
        .map {
          _.leftMap(_.asJava).fold(
            messages.Either.forLeft(_),
            messages.Either.forRight(_),
          )
        }
    )

  @JsonRequest("textDocument/diagnostic")
  def diagnostic(
    params: DocumentDiagnosticParams
  ): CompletableFuture[DocumentDiagnosticReport] = d.unsafeToCompletableFuture(
    impl.diagnostic(params)
  )

  @JsonRequest("textDocument/codeLens")
  def codeLens(
    params: CodeLensParams
  ): CompletableFuture[java.util.List[CodeLens]] = d.unsafeToCompletableFuture(
    impl.codeLens(params).map(_.asJava)
  )

  @JsonRequest("workspace/executeCommand")
  def executeCommand(params: ExecuteCommandParams): CompletableFuture[Object] = d
    .unsafeToCompletableFuture(
      impl
        .executeCommand(params)
        .as(null: Object)
    )

  @JsonNotification("workspace/didChangeWatchedFiles")
  def didChangeWatchedFiles(
    params: DidChangeWatchedFilesParams
  ): Unit = d.unsafeRunSync(impl.didChangeWatchedFiles(params))

  @JsonRequest("textDocument/documentSymbol")
  @ResponseJsonAdapter(classOf[DocumentSymbolResponseAdapter])
  def documentSymbol(
    params: DocumentSymbolParams
  ): CompletableFuture[java.util.List[messages.Either[Nothing, DocumentSymbol]]] = d
    .unsafeToCompletableFuture(
      impl.documentSymbol(params).map(_.map(messages.Either.forRight(_)).asJava)
    )

  @JsonRequest("smithyql/runQuery")
  def runQuery(params: RunQueryParams): CompletableFuture[Object] = executeCommand(
    new ExecuteCommandParams()
      .tap(_.setCommand(playground.language.Command.RUN_QUERY))
      .tap(_.setArguments(List(new JsonPrimitive(params.uri): Object).asJava))
  )

  @JsonRequest("exit")
  def exit(): CompletableFuture[Object] = d.unsafeToCompletableFuture(impl.exit.as(null: Object))
}
