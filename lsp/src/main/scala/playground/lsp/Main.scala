package playground.lsp

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.kernel.Deferred
import cats.effect.std.Dispatcher
import org.eclipse.lsp4j.MessageParams
import org.eclipse.lsp4j.MessageType
import org.eclipse.lsp4j.launch.LSPLauncher

import java.io.File
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintWriter
import org.eclipse.lsp4j.services

object Main extends IOApp.Simple {

  private val logWriter = new PrintWriter(new File("lsp-log.txt"))

  def log(s: String): IO[Unit] = IO(logWriter.println(s))

  def run: IO[Unit] =
    Dispatcher[IO].use { implicit d =>
      launch(System.in, System.out)
    } *>
      log("Server terminated without errors")

  def launch(
    in: InputStream,
    out: OutputStream,
  )(
    implicit d: Dispatcher[IO]
  ) = TextDocumentManager.instance[IO].flatMap { implicit tdm =>
    Deferred[IO, services.LanguageClient].flatMap { clientPromise =>
      implicit val client: LanguageClient[IO] = LanguageClient.adapt(clientPromise.get)

      val server = new PlaygroundLanguageServerAdapter(LanguageServer.instance[IO])

      val launcher = new LSPLauncher.Builder[services.LanguageClient]()
        .setLocalService(server)
        .setRemoteInterface(classOf[services.LanguageClient])
        .setInput(in)
        .setOutput(out)
        .traceMessages(logWriter)
        .create();

      connect(launcher.getRemoteProxy(), clientPromise) *>
        IO.interruptibleMany(launcher.startListening().get())
    }
  }

  private def connect(
    client: services.LanguageClient,
    clientDeff: Deferred[IO, services.LanguageClient],
  ) =
    log("connecting: " + client) *>
      clientDeff.complete(client) *>
      IO(client.showMessage(new MessageParams(MessageType.Info, "hello from smithyql server"))) *>
      log("Server connected")

}
