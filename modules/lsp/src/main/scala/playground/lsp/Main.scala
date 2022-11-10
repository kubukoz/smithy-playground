package playground.lsp

import cats.Show
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.implicits._
import cats.effect.kernel.Deferred
import cats.effect.std
import cats.effect.std.Dispatcher
import cats.implicits._
import fs2.io.net.Network
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.launch.LSPLauncher

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
    import com.comcast.ip4s._

    Network[IO]
      .server(port = Some(port"8887"))
      .map { sock =>
        fs2
          .Stream
          .exec(IO.println("new client connected")) ++
          sock
            .reads
            .through(fs2.io.toInputStream[IO])
            .flatMap { input =>
              val drain =
                fs2
                  .io
                  .readOutputStream[IO](4096)(os =>
                    launch(input, os)
                      .use { l =>
                        IO.interruptibleMany(l.startListening().get())
                      }
                      .onCancel(IO.println("this is being cancelled"))
                      .void
                  )
              drain.through(sock.writes)
            }
            .onFinalize(IO.println("client disconnected"))
      }
      .parJoin(10)
      .compile
      .drain
  }

  private def launch(
    in: InputStream,
    out: OutputStream,
  ) = Deferred[IO, LanguageClient[IO]].toResource.flatMap { clientRef =>
    implicit val lc: LanguageClient[IO] = LanguageClient.defer(clientRef.get)

    MainServer
      .makeServer[IO]
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
            .as(launcher: Launcher[_])
      }
  }

  private def launchWS(
    in: InputStream,
    out: OutputStream,
  ) = Deferred[IO, LanguageClient[IO]].toResource.flatMap { clientRef =>
    implicit val lc: LanguageClient[IO] = LanguageClient.defer(clientRef.get)

    MainServer
      .makeServer[IO]
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
          .create()

        log[IO]("connecting") *>
          clientRef.complete(LanguageClient.adapt[IO](launcher.getRemoteProxy())) *>
          log[IO]("Server connected")
            .as(launcher)
      }
  }

}
