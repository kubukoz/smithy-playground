package playground.e2e

import buildinfo.BuildInfo
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import fs2.io.file
import fs2.io.process.Processes
import jsonrpclib.fs2.FS2Channel
import jsonrpclib.fs2.given
import langoustine.lsp.Communicate
import langoustine.lsp.LSPBuilder
import langoustine.lsp.requests.exit
import langoustine.lsp.requests.initialize
import langoustine.lsp.requests.shutdown
import langoustine.lsp.requests.window
import langoustine.lsp.runtime.Opt
import langoustine.lsp.runtime.Uri
import weaver.*

import scala.concurrent.duration.*

object E2ETests extends SimpleIOSuite {

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
              process
                .stdout
                .through(jsonrpclib.fs2.lsp.decodeMessages[IO])
                .through(channel.inputOrBounce)
                .concurrently(
                  channel
                    .output
                    .through(jsonrpclib.fs2.lsp.encodeMessages[IO])
                    .through(process.stdin)
                )
                .concurrently(
                  process
                    .stderr
                    .through(fs2.io.stderr[IO])
                )
                .compile
                .drain
                .background
                .as((comms, channel))
            }
        }
        // fs2's process finalizer is `destroy(); waitFor()`, which blocks forever if the
        // server ignores SIGTERM. Ask it to exit first, and don't wait for it indefinitely.
        .flatMap { case (comms, channel) =>
          Resource
            .onFinalize {
              comms
                .request(shutdown(()))
                .attempt
                .productR(sendExit(channel).attempt)
                .productR(process.exitValue.void)
                .timeoutTo(10.seconds, IO.unit)
            }
            .as(comms)
        }
    }

  // Langoustine's `exit(())` serializes its Unit params as `null`, which jsonrpclib parses
  // back as `None` - and its own decoder rejects that with "missing payload". The failure
  // lands in `FS2Channel.reportError`, which is `???`, killing the server before it can
  // shut down. Sending `{}` instead decodes as `Some`, which reads back as Unit just fine.
  private def sendExit(channel: FS2Channel[IO]): IO[Unit] = {
    given jsonrpclib.Codec[Unit] =
      new jsonrpclib.Codec[Unit] {
        def encode(a: Unit): jsonrpclib.Payload = jsonrpclib.Payload("{}".getBytes)

        def decode(payload: Option[jsonrpclib.Payload]): Either[jsonrpclib.ProtocolError, Unit] =
          Right(())
      }

    channel.notificationStub[Unit](exit.notificationMethod).apply(())
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
      .timeout(60.seconds)
  }

}
