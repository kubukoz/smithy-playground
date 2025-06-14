package playground.e2e

import buildinfo.BuildInfo
import cats.effect.Concurrent
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import fs2.io.file
import fs2.io.process.Processes
import jsonrpclib.fs2.FS2Channel
import jsonrpclib.fs2.given
import langoustine.lsp.Communicate
import langoustine.lsp.LSPBuilder
import langoustine.lsp.requests.initialize
import langoustine.lsp.requests.window
import langoustine.lsp.runtime.Opt
import langoustine.lsp.runtime.Uri
import weaver.*

import java.io.PrintWriter
import java.lang.ProcessBuilder.Redirect
import java.util.concurrent.CompletableFuture
import scala.annotation.nowarn
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.chaining.*

object E2ETests extends SimpleIOSuite {

  private def runServer: Resource[IO, Communicate[IO]] = Processes[IO]
    .spawn(fs2.io.process.ProcessBuilder("cs", "launch", BuildInfo.lspArtifact))
    .onFinalize(IO.println("Server process finalized"))
    .flatMap { process =>
      val clientEndpoints: LSPBuilder[IO] => LSPBuilder[IO] =
        _.handleNotification(window.showMessage) { in =>
          val messageParams = in.params
          IO.println {
            s"${Console.MAGENTA}Message from server: ${messageParams.message} (type: ${messageParams.`type`.name})${Console.RESET}"
          }
        }

      FS2Channel[IO]()
        .compile
        .resource
        .onlyOrError
        .onFinalize(IO.println("Channel finalized"))
        .flatMap { chan =>
          val comms = Communicate.channel(chan)
          chan
            .withEndpoints(clientEndpoints(LSPBuilder.create[IO]).build(comms))
            .flatMap { channel =>
              fs2
                .Stream
                .eval(IO.never)
                .concurrently(
                  process
                    .stdout
                    // fs2.io.stdout seems to be printed repeatedly for some reason, could be a bug with sbt
                    .observe(_.through(fs2.text.utf8.decode[IO]).debug("stdout: " + _).drain)
                    .through(jsonrpclib.fs2.lsp.decodeMessages[IO])
                    .through(channel.inputOrBounce)
                    .onFinalize(IO.println("stdout stream finalized"))
                )
                .concurrently(
                  channel
                    .output
                    .through(jsonrpclib.fs2.lsp.encodeMessages[IO])
                    .observe(_.through(fs2.text.utf8.decode[IO]).debug("stdin: " + _).drain)
                    .through(process.stdin)
                    .onFinalize(IO.println("stdin stream finalized"))
                )
                .concurrently(
                  process
                    .stderr
                    .through(fs2.io.stderr[IO])
                    .onFinalize(
                      IO.println("stderr stream finalized")
                    )
                )
                .onFinalize(
                  IO.println("stdio streams finalized")
                )
                .compile
                .drain
                .background
                .as(comms)
                .onFinalize(
                  IO.println("Server process and channel finalized")
                )
            }
        }
    }

  private def initializeParams(
    workspaceFolders: List[file.Path]
  ): langoustine.lsp.structures.InitializeParams = langoustine
    .lsp
    .structures
    .InitializeParams(
      processId = Opt.empty,
      rootUri = Opt.empty,
      capabilities = langoustine.lsp.structures.ClientCapabilities(),
      workspaceFolders = Opt(
        Opt(
          workspaceFolders
            .zipWithIndex
            .map { case (path, i) =>
              langoustine
                .lsp
                .structures
                .WorkspaceFolder(
                  uri = Uri(path.toNioPath.toUri().toString),
                  name = s"test-workspace-$i",
                )
            }
            .toVector
        )
      ),
    )

  test("server startup and initialize") {
    runServer
      .use { ls =>
        file.Files[IO].tempDirectory.use { tempDirectory =>
          val initParams = initializeParams(workspaceFolders = List(tempDirectory))

          ls.request(initialize(initParams)).map { result =>
            expect.eql(
              result.serverInfo.toOption.get.name,
              "Smithy Playground",
            )
          }
        } <* IO.println("Finished inner test")
      }
      .timeout(20.seconds) <* IO.println("Finished test and closed server")
  }
}
