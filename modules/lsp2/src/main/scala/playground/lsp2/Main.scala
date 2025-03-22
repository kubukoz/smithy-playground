package playground.lsp2

import cats.Functor
import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.effect.kernel.Resource
import cats.kernel.Semigroup
import cats.syntax.all.*
import jsonrpclib.Channel
import jsonrpclib.Endpoint
import jsonrpclib.Monadic
import jsonrpclib.fs2.given
import langoustine.lsp.Communicate
import langoustine.lsp.Invocation
import langoustine.lsp.LSPBuilder
import langoustine.lsp.app.LangoustineApp
import langoustine.lsp.enumerations.MessageType
import langoustine.lsp.enumerations.TextDocumentSyncKind
import langoustine.lsp.requests.CustomNotification
import langoustine.lsp.requests.LSPNotification
import langoustine.lsp.requests.LSPRequest
import langoustine.lsp.requests.initialize
import langoustine.lsp.requests.window
import langoustine.lsp.requests.workspace
import langoustine.lsp.runtime.Opt
import langoustine.lsp.structures.CodeLensOptions
import langoustine.lsp.structures.CompletionOptions
import langoustine.lsp.structures.DiagnosticOptions
import langoustine.lsp.structures.InitializeResult
import langoustine.lsp.structures.InitializeResult.ServerInfo
import langoustine.lsp.structures.LogMessageParams
import langoustine.lsp.structures.ServerCapabilities
import langoustine.lsp.structures.ShowMessageParams
import playground.lsp.LanguageClient
import playground.lsp.MainServer
import playground.lsp.ServerCapabilitiesCompiler

object Main extends LangoustineApp {

  def server(args: List[String]): Resource[IO, LSPBuilder[IO]] = IO
    .deferred[LanguageClient[IO]]
    .toResource
    .flatMap { clientRef =>
      implicit val lc: LanguageClient[IO] = LanguageClient.defer(clientRef.get)

      MainServer
        .makeServer[IO]
        .map { server =>
          LSPBuilder
            .create[IO]
            .handleRequest(initialize) { req =>
              server
                .initialize(
                  req.params.workspaceFolders.toOption.foldMap(_.toOption.orEmpty).toList.map {
                    workspaceFolder =>
                      playground.language.Uri.fromUriString(workspaceFolder.uri.value)
                  }
                )
                .map { result =>
                  InitializeResult(
                    capabilities = result
                      .serverCapabilities(new ServerCapabilitiesCompiler.Default {
                        type Result = ServerCapabilities => ServerCapabilities

                        def default: Result = identity

                        def semigroup: Semigroup[ServerCapabilities => ServerCapabilities] =
                          _.andThen(_)

                        // def codeLensProvider: Result =
                        //   _.copy(codeLensProvider = Opt(CodeLensOptions()))

                        // def completionProvider: Result =
                        //   _.copy(completionProvider = Opt(CompletionOptions()))

                        // def diagnosticProvider: Result =
                        //   _.copy(diagnosticProvider =
                        //     Opt(
                        //       DiagnosticOptions(
                        //         interFileDependencies = false,
                        //         workspaceDiagnostics = false,
                        //       )
                        //     )
                        //   )

                        // def documentFormattingProvider: Result =
                        //   _.copy(documentFormattingProvider = Opt(true))

                        // def documentSymbolProvider: Result =
                        //   _.copy(documentSymbolProvider = Opt(true))

                        override def textDocumentSync(kind: playground.lsp.TextDocumentSyncKind)
                          : Result =
                          _.copy(textDocumentSync = Opt(kind match {
                            case playground.lsp.TextDocumentSyncKind.Full =>
                              TextDocumentSyncKind.Full
                          }))

                      })
                      .apply(ServerCapabilities()),
                    serverInfo = Opt(
                      ServerInfo(
                        name = result.serverInfo.name,
                        version = Opt(result.serverInfo.version),
                      )
                    ),
                  )
                }
            }
        }
        .map(bindClient(_, clientRef))
    }

  private def bindClient(lsp: LSPBuilder[IO], clientDef: Deferred[IO, LanguageClient[IO]])
    : LSPBuilder[IO] =
    new LSPBuilder[IO] {
      export lsp.build
      export lsp.handleNotification
      export lsp.handleRequest

      override def bind[T <: Channel[IO]](
        channel: T,
        communicate: Communicate[IO],
      )(
        using Monadic[IO]
      ): IO[T] =
        clientDef.complete(ClientAdapter.adapt(communicate)) *>
          lsp.bind(channel, communicate)
    }

}

object ClientAdapter {

  def adapt[F[_]: Functor](comms: Communicate[F]): LanguageClient[F] =
    new LanguageClient[F] {
      def logOutput(msg: String): F[Unit] = comms.notification(
        window.logMessage(LogMessageParams(`type` = MessageType.Info, message = msg))
      )

      def configuration[A](v: playground.lsp.ConfigurationValue[A]): F[A] = ??? // for later

      def refreshCodeLenses: F[Unit] = comms.request(workspace.codeLens.refresh(())).void
      def refreshDiagnostics: F[Unit] = comms.request(workspace.diagnostic.refresh(())).void

      def showMessage(tpe: playground.lsp.MessageType, msg: String): F[Unit] = comms.notification(
        window.showMessage(
          ShowMessageParams(
            `type` =
              tpe match {
                case playground.lsp.MessageType.Error   => MessageType.Error
                case playground.lsp.MessageType.Info    => MessageType.Info
                case playground.lsp.MessageType.Warning => MessageType.Warning
              },
            message = msg,
          )
        )
      )

      def showOutputPanel: F[Unit] = comms.notification(smithyql.showOutputPanel(()))
    }

  object smithyql {
    object showOutputPanel extends CustomNotification[Unit]("smithyql/showOutputPanel")
  }

}
