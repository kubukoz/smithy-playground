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
import org.eclipse.lsp4j.ClientCapabilities
import org.eclipse.lsp4j.InitializeParams
import org.eclipse.lsp4j.InitializeResult
import org.eclipse.lsp4j.MessageActionItem
import org.eclipse.lsp4j.MessageParams
import org.eclipse.lsp4j.PublishDiagnosticsParams
import org.eclipse.lsp4j.ShowMessageRequestParams
import org.eclipse.lsp4j.WorkspaceFolder
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services.LanguageServer
import playground.e2e.E2ETests.LanguageServerAdapter
import playground.lsp.PlaygroundLanguageClient
import weaver.*

import java.io.PrintWriter
import java.lang.ProcessBuilder.Redirect
import java.util.concurrent.CompletableFuture
import scala.annotation.nowarn
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.chaining.*

object E2ETests extends SimpleIOSuite {

  class LanguageServerAdapter(
    val ls: LanguageServer
  ) {

    def initialize(
      params: InitializeParams
    ): IO[InitializeResult] = IO.fromCompletableFuture(IO(ls.initialize(params)))

  }

  private def runServer: Resource[IO, Communicate[IO]] = Processes[IO]
    .spawn(fs2.io.process.ProcessBuilder("cs", "launch", BuildInfo.lspArtifact))
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
        .flatMap { chan =>
          val comms = Communicate.channel(chan)
          chan
            .withEndpoints(clientEndpoints(LSPBuilder.create[IO]).build(comms))
            .flatMap { channel =>
              fs2
                .Stream
                .never[IO]
                .concurrently(
                  process
                    .stdout
                    // fs2.io.stdout seems to be printed repeatedly for some reason, could be a bug with sbt
                    .observe(_.through(fs2.text.utf8.decode[IO]).debug("stdout: " + _).drain)
                    .through(jsonrpclib.fs2.lsp.decodeMessages[IO])
                    .through(channel.inputOrBounce)
                )
                .concurrently(
                  channel
                    .output
                    .through(jsonrpclib.fs2.lsp.encodeMessages[IO])
                    .observe(_.through(fs2.text.utf8.decode[IO]).debug("stdin: " + _).drain)
                    .through(process.stdin)
                )
                .concurrently(process.stderr.through(fs2.io.stderr[IO]))
                .compile
                .drain
                .background
                .as(comms)
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
        }
      }
      .timeout(20.seconds)
  }
}
