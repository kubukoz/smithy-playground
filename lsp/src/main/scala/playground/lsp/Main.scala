package playground.lsp

import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services.LanguageClient

import java.io.File
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintWriter
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.kernel.Deferred

object Main extends IOApp.Simple {

  private val logWriter = new PrintWriter(new File("lsp-log.txt"))

  def log(s: String): IO[Unit] = IO(logWriter.println(s))

  def run: IO[Unit] =
    launch(System.in, System.out) *>
      log("Server terminated without errors")

  def launch(in: InputStream, out: OutputStream) = Deferred[IO, LanguageClient].flatMap {
    clientPromise =>
      val server = new MyLanguageServer(clientPromise)

      val launcher = new LSPLauncher.Builder[LanguageClient]()
        .setLocalService(server)
        .setRemoteInterface(classOf[LanguageClient])
        .setInput(in)
        .setOutput(out)
        .traceMessages(logWriter)
        .create();

      val client = launcher.getRemoteProxy()

      server.connect(client) *>
        log("Server connected") *>
        IO.interruptibleMany(launcher.startListening().get())
  }

}
