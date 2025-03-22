package playground.lsp2

import cats.Functor
import cats.kernel.Semigroup
import cats.syntax.all.*
import langoustine.lsp.LSPBuilder
import langoustine.lsp.enumerations.TextDocumentSyncKind
import langoustine.lsp.requests.initialize
import langoustine.lsp.runtime.Opt
import langoustine.lsp.structures.CodeLensOptions
import langoustine.lsp.structures.CompletionOptions
import langoustine.lsp.structures.DiagnosticOptions
import langoustine.lsp.structures.InitializeResult
import langoustine.lsp.structures.InitializeResult.ServerInfo
import langoustine.lsp.structures.ServerCapabilities
import playground.lsp.ServerCapabilitiesCompiler

object LangoustineServerAdapter {

  def adapt[F[_]: Functor](server: playground.lsp.LanguageServer[F])
    : LSPBuilder[F] => LSPBuilder[F] =
    _.handleRequest(initialize) { req =>
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

                def semigroup: Semigroup[ServerCapabilities => ServerCapabilities] = _.andThen(_)

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

                override def textDocumentSync(kind: playground.lsp.TextDocumentSyncKind): Result =
                  _.copy(textDocumentSync = Opt(kind match {
                    case playground.lsp.TextDocumentSyncKind.Full => TextDocumentSyncKind.Full
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
