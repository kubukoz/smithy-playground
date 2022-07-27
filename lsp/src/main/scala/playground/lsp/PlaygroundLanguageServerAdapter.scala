package playground.lsp

import cats.Functor
import cats.effect.std.Dispatcher
import cats.implicits._
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest

import java.util.concurrent.CompletableFuture
import scala.jdk.CollectionConverters._
import org.eclipse.lsp4j.jsonrpc.messages

final class PlaygroundLanguageServerAdapter[F[_]: Functor](
  impl: LanguageServer[F]
)(
  implicit d: Dispatcher[F]
) {

  @JsonRequest("initialize")
  def initialize(
    params: InitializeParams
  ): CompletableFuture[InitializeResult] = d.unsafeToCompletableFuture(impl.initialize(params))

  @JsonRequest("shutdown")
  def shutdown(
  ): CompletableFuture[Object] = d.unsafeToCompletableFuture(impl.shutdown().as(null: Object))

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

  @JsonRequest("exit")
  def exit(): CompletableFuture[Object] = d.unsafeToCompletableFuture(impl.exit().as(null: Object))
}
