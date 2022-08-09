package playground.lsp

import cats.Show
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.effect.kernel.Deferred
import cats.effect.kernel.Resource
import cats.effect.std
import cats.effect.std.Dispatcher
import cats.implicits._
import org.eclipse.lsp4j.launch.LSPLauncher
import playground.TextDocumentManager

import java.io.File
import java.io.FileOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import java.io.PrintWriter
import java.nio.charset.Charset

object Main extends IOApp.Simple {

  private val logOut = new PrintStream(new FileOutputStream(new File("smithyql-log.txt")))
  private val logWriter = new PrintWriter(logOut)

  implicit val ioConsole: std.Console[IO] =
    new std.Console[IO] {

      def readLineWithCharset(
        charset: Charset
      ): IO[String] = IO.consoleForIO.readLineWithCharset(charset)

      def print[A](a: A)(implicit S: Show[A]): IO[Unit] = IO(logWriter.print(a.show))

      def println[A](a: A)(implicit S: Show[A]): IO[Unit] = IO(logWriter.println(a.show))

      def error[A](a: A)(implicit S: Show[A]): IO[Unit] = IO(logWriter.print("ERROR: " + a.show))

      def errorln[A](a: A)(implicit S: Show[A]): IO[Unit] = IO(
        logWriter.println("ERROR: " + a.show)
      )

    }

  def log[F[_]: std.Console](s: String): F[Unit] = std.Console[F].println(s)

  def run: IO[Unit] = {
    val stdin = System.in
    val stdout = System.out

    IO(System.setOut(logOut)).toResource *>
      launch(stdin, stdout)
  }
    .use { launcher =>
      IO.interruptibleMany(launcher.startListening().get())
    } *> log("Server terminated without errors")

  def launch(
    in: InputStream,
    out: OutputStream,
  ) = Deferred[IO, LanguageClient[IO]].toResource.flatMap { clientRef =>
    implicit val lc: LanguageClient[IO] = LanguageClient.defer(clientRef.get)

    makeServer[IO]
      .flatMap { server =>
        Dispatcher[IO].map(implicit d => new PlaygroundLanguageServerAdapter(server))
      }
      .evalMap { serverAdapter =>
        val launcher = new LSPLauncher.Builder[PlaygroundLanguageClient]()
          .setLocalService(serverAdapter)
          .setRemoteInterface(classOf[PlaygroundLanguageClient])
          .setInput(in)
          .setOutput(out)
          .traceMessages(logWriter)
          .create();

        log[IO]("connecting") *>
          clientRef.complete(LanguageClient.adapt[IO](launcher.getRemoteProxy())) *>
          log[IO]("Server connected")
            .as(launcher)
      }
  }

  private def makeServer[F[_]: Async: std.Console](
    implicit lc: LanguageClient[F]
  ): Resource[F, LanguageServer[F]] = TextDocumentManager
    .instance[F]
    .toResource
    .flatMap { implicit tdm =>
      implicit val buildLoader: BuildLoader[F] = BuildLoader.instance[F]

      ServerBuilder
        .instance[F]
        .evalMap { implicit builder =>
          ServerLoader.instance[F]
        }
        .map(_.server)
    }

}
