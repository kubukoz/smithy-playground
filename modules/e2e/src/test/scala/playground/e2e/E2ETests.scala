package playground.e2e

import buildinfo.BuildInfo
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import fs2.io.file
import jsonrpclib.fs2.FS2Channel
import jsonrpclib.fs2.given
import langoustine.lsp.Communicate
import langoustine.lsp.requests.initialize
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

  private def runServer2: Resource[IO, Communicate[IO]] =
    // val client: PlaygroundLanguageClient =
    //   new PlaygroundLanguageClient {

    //     override def telemetryEvent(
    //       `object`: Object
    //     ): Unit = ()

    //     override def publishDiagnostics(
    //       diagnostics: PublishDiagnosticsParams
    //     ): Unit = ()

    //     override def showMessage(
    //       messageParams: MessageParams
    //     ): Unit = println(
    //       s"${Console.MAGENTA}Message from server: ${messageParams
    //           .getMessage()} (type: ${messageParams.getType()})${Console.RESET}"
    //     )

    //     override def showMessageRequest(
    //       requestParams: ShowMessageRequestParams
    //     ): CompletableFuture[MessageActionItem] = (IO.stub: IO[MessageActionItem])
    //       .unsafeToCompletableFuture()

    //     override def logMessage(
    //       message: MessageParams
    //     ): Unit = ()

    //     override def showOutputPanel(
    //     ): Unit = ()

    //   }

    // val builder =
    //   new ProcessBuilder(
    //     "cs",
    //     "launch",
    //     BuildInfo.lspArtifact,
    //   )
    //     // Watch process stderr in test runner
    //     .redirectError(Redirect.INHERIT)

    // Resource
    //   .make(IO.interruptibleMany(builder.start()))(p => IO(p.destroy()).void)
    //   .flatMap { process =>
    //     val launcher = new LSPLauncher.Builder[LanguageServer]()
    //       .setLocalService(client)
    //       .setRemoteInterface(classOf[LanguageServer])
    //       .setInput(process.getInputStream())
    //       .setOutput(process.getOutputStream())
    //       .traceMessages(new PrintWriter(System.err))
    //       .create()

    //     Resource
    //       .make(IO(launcher.startListening()).timeout(5.seconds))(f =>
    //         IO(f.cancel(true): @nowarn("msg=discarded non-Unit"))
    //       )
    //       .as(new LanguageServerAdapter(launcher.getRemoteProxy()))
    //   }

    FS2Channel[IO]().compile.resource.onlyOrError.map { channel =>
      Communicate.channel(channel)
    }

  private def runServer: Resource[IO, LanguageServerAdapter] = {

    val client: PlaygroundLanguageClient =
      new PlaygroundLanguageClient {

        override def telemetryEvent(
          `object`: Object
        ): Unit = ()

        override def publishDiagnostics(
          diagnostics: PublishDiagnosticsParams
        ): Unit = ()

        override def showMessage(
          messageParams: MessageParams
        ): Unit = println(
          s"${Console.MAGENTA}Message from server: ${messageParams
              .getMessage()} (type: ${messageParams.getType()})${Console.RESET}"
        )

        override def showMessageRequest(
          requestParams: ShowMessageRequestParams
        ): CompletableFuture[MessageActionItem] = (IO.stub: IO[MessageActionItem])
          .unsafeToCompletableFuture()

        override def logMessage(
          message: MessageParams
        ): Unit = ()

        override def showOutputPanel(
        ): Unit = ()

      }

    val builder =
      new ProcessBuilder(
        "cs",
        "launch",
        BuildInfo.lspArtifact,
      )
        // Watch process stderr in test runner
        .redirectError(Redirect.INHERIT)

    Resource
      .make(IO.interruptibleMany(builder.start()))(p => IO(p.destroy()).void)
      .flatMap { process =>
        val launcher = new LSPLauncher.Builder[LanguageServer]()
          .setLocalService(client)
          .setRemoteInterface(classOf[LanguageServer])
          .setInput(process.getInputStream())
          .setOutput(process.getOutputStream())
          .traceMessages(new PrintWriter(System.err))
          .create()

        Resource
          .make(IO(launcher.startListening()).timeout(5.seconds))(f =>
            IO(f.cancel(true): @nowarn("msg=discarded non-Unit"))
          )
          .as(new LanguageServerAdapter(launcher.getRemoteProxy()))
      }
  }

  private def initializeParams(
    workspaceFolders: List[file.Path]
  ): InitializeParams = new InitializeParams()
    .tap(
      _.setWorkspaceFolders(
        workspaceFolders
          .zipWithIndex
          .map { case (path, i) =>
            new WorkspaceFolder(path.toNioPath.toUri().toString(), s"test-workspace-$i")
          }
          .asJava
      )
    )
    .tap(_.setCapabilities(new ClientCapabilities()))

  private def initializeParams2(
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

  test("server startup and initialize 2") {
    runServer2
      .use { ls =>
        file.Files[IO].tempDirectory.use { tempDirectory =>
          val initParams = initializeParams2(workspaceFolders = List(tempDirectory))

          ls.request(initialize(initParams)).map { result =>
            assert.eql(
              result.serverInfo.toOption.get.name,
              "Smithy Playground",
            )
          }
        }
      }
      .timeout(20.seconds)
  }

  // test("server startup and initialize") {
  //   runServer
  //     .use { ls =>
  //       file.Files[IO].tempDirectory.use { tempDirectory =>
  //         val initParams = initializeParams(workspaceFolders = List(tempDirectory))

  //         ls.initialize(initParams).map { result =>
  //           assert.eql(
  //             result.getServerInfo().getName(),
  //             "Smithy Playground",
  //           )
  //         } <* IO(ls.ls.exit())
  //       }

  //     }
  //     .timeout(20.seconds)
  // }
}
