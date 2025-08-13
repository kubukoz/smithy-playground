package playground.lsp

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.kernel.Deferred
import cats.effect.kernel.Resource
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import org.eclipse.lsp4j.launch.LSPLauncher

import java.io.File
import java.io.FileOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import java.io.PrintWriter

object Main extends IOApp.Simple {
  private val logOut = new PrintStream(new FileOutputStream(new File("smithyql-log.txt")))
  private val logWriter = new PrintWriter(logOut)
  private val stdin_raw = System.in
  private val stdout_raw = System.out

  def run: IO[Unit] =
    IO(System.setOut(logOut)) *>
      launch(stdin_raw, stdout_raw).useEval *>
      IO.println("Server terminated without errors")

  def launch(
    in: InputStream,
    out: OutputStream,
  ): Resource[IO, IO[Unit]] = Deferred[IO, LanguageServer[IO]]
    .toResource
    .flatMap { serverRef =>
      given LanguageServer[IO] = LanguageServer.defer(
        serverRef.get
      )

      Dispatcher
        .sequential[IO]
        .map(implicit d => new PlaygroundLanguageServerAdapter(summon[LanguageServer[IO]]))
        .flatMap { serverAdapter =>
          val launcher = new LSPLauncher.Builder[PlaygroundLanguageClient]()
            .setLocalService(serverAdapter)
            .setRemoteInterface(classOf[PlaygroundLanguageClient])
            .setInput(in)
            .setOutput(out)
            .traceMessages(logWriter)
            .create()

          given LanguageClient[IO] = PlaygroundLanguageClientAdapter.adapt[IO](
            launcher.getRemoteProxy()
          )

          // Start listening, in the meantime construct a server
          IO.interruptibleMany(launcher.startListening().get()).background.map(_.void) <&
            MainServer
              .makeServer[IO]
              .evalMap { server =>
                serverRef.complete(server).void
              }
        }
    }

}
