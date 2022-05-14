package playground.lsp

import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.effect.unsafe.implicits._
import cats.implicits._
import com.google.gson.JsonElement
import fs2.io.file.Files
import fs2.io.file.Path
import org.eclipse.lsp4j
import org.eclipse.lsp4j.DocumentFormattingParams
import org.eclipse.lsp4j.InitializeParams
import org.eclipse.lsp4j.InitializeResult
import org.eclipse.lsp4j.MessageParams
import org.eclipse.lsp4j.MessageType
import org.eclipse.lsp4j.ServerCapabilities
import org.eclipse.lsp4j.TextDocumentSyncKind
import org.eclipse.lsp4j.TextEdit
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest
import org.eclipse.lsp4j.services.LanguageClient
import playground.smithyql.Formatter
import playground.smithyql.SmithyQLParser

import java.net.URI
import java.nio.file.Paths
import java.util.concurrent.CompletableFuture
import scala.annotation.nowarn
import scala.jdk.CollectionConverters._

import util.chaining._

final class MyLanguageServer(client: Deferred[IO, LanguageClient]) {

  @JsonRequest("initialize")
  def initialize(
    @nowarn(
      "msg=parameter value ignored in method initialize is never used"
    ) ignored: InitializeParams
  ): CompletableFuture[InitializeResult] = {
    val capabilities = new ServerCapabilities()
      .tap(_.setTextDocumentSync(TextDocumentSyncKind.Full))
      .tap(_.setDocumentFormattingProvider(true))

    new InitializeResult(capabilities).pure[IO]
  }.unsafeToCompletableFuture()

  @JsonRequest("shutdown")
  def shutdown(): CompletableFuture[Object] = IO.unit.as(null: Object).unsafeToCompletableFuture()

  @JsonRequest("textDocument/formatting")
  def formatting(
    params: DocumentFormattingParams
  ): CompletableFuture[java.util.List[TextEdit]] = Files[IO]
    .readAll(Path.fromNioPath(Paths.get(new URI(params.getTextDocument().getUri()))))
    .through(fs2.text.utf8.decode[IO])
    .compile
    .string
    .flatMap { text =>
      client
        .get
        .flatMap { client =>
          IO.fromCompletableFuture {
            IO {
              client.configuration(
                new lsp4j.ConfigurationParams(
                  List(
                    new lsp4j.ConfigurationItem().tap(_.setSection("smithyql.formatter.maxWidth"))
                  ).asJava
                )
              )
            }
          }
        }
        .map(_.asScala.toList)
        .flatMap {
          case (width: JsonElement) :: Nil => IO.pure(width.getAsInt())
          case r                           => IO.raiseError(new Throwable("config error: " + r))
        }
        .flatMap { maxWidth =>
          SmithyQLParser
            .parseFull(text)
            .liftTo[IO]
            .map { parsed =>
              val formatted = Formatter.format(parsed, maxWidth)

              val lines = text.linesWithSeparators.toList

              List(
                new TextEdit(
                  new lsp4j.Range(
                    new lsp4j.Position(0, 0),
                    new lsp4j.Position(lines.indices.last, lines.last.size),
                  ),
                  formatted,
                )
              )
            }
        }
    }
    .map(_.asJava)
    .unsafeToCompletableFuture()

  @JsonRequest("exit")
  def exit(): CompletableFuture[Object] = IO.unit.as(null: Object).unsafeToCompletableFuture()

  def connect(clientImpl: LanguageClient): IO[Unit] = {
    Main.log("connecting: " + client)

    client.complete(clientImpl) *>
      IO(clientImpl.showMessage(new MessageParams(MessageType.Info, "hello from smithyql server")))
  }

}
