package playground.lsp

import cats.Show
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.implicits._
import cats.effect.kernel.Deferred
import cats.effect.std
import cats.effect.std.Dispatcher
import cats.implicits._
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
    (
      IO(System.setOut(logOut)).toResource *>
        launch(stdin_raw, stdout_raw)
    )
      .use { launcher =>
        println(System.out)
        println(Console.out)
        IO.interruptibleMany(launcher.startListening().get())
      } *> IO.println("Server terminated without errors")

  def launch(
    in: InputStream,
    out: OutputStream,
  ) = Deferred[IO, LanguageClient[IO]].toResource.flatMap { clientRef =>
    implicit val lc: LanguageClient[IO] = LanguageClient.defer(clientRef.get)

    MainServer
      .makeServer[IO]
      .flatMap { server =>
        Dispatcher.sequential[IO].map(implicit d => new PlaygroundLanguageServerAdapter(server))
      }
      .evalMap { serverAdapter =>
        val launcher = new LSPLauncher.Builder[PlaygroundLanguageClient]()
          .setLocalService(serverAdapter)
          .setRemoteInterface(classOf[PlaygroundLanguageClient])
          .setInput(in)
          .setOutput(out)
          .traceMessages(logWriter)
          .create();

        IO.println("connecting") *>
          clientRef.complete(LanguageClient.adapt[IO](launcher.getRemoteProxy())) *>
          IO.println("Server connected")
            .as(launcher)
      }
  }

}
