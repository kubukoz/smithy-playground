package playground.lsp2

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.effect.kernel.Resource
import jsonrpclib.Channel
import jsonrpclib.Monadic
import jsonrpclib.fs2.given
import langoustine.lsp.Communicate
import langoustine.lsp.LSPBuilder
import langoustine.lsp.app.LangoustineApp
import langoustine.lsp.requests.LSPNotification
import langoustine.lsp.requests.LSPRequest

object Main extends LangoustineApp {

  override def run(args: List[String]): IO[ExitCode] = super
    .run(args)
    .guaranteeCase(ec => IO.consoleForIO.errorln(s"Server terminated with result: $ec"))

  def server(args: List[String]): Resource[IO, LSPBuilder[IO]] = IO
    .deferred[playground.lsp.LanguageClient[IO]]
    .toResource
    .flatMap { clientRef =>
      implicit val lc
        : playground.lsp.LanguageClient[IO] = playground.lsp.LanguageClient.defer(clientRef.get)

      playground
        .lsp
        .MainServer
        .makeServer[IO]
        .map(LangoustineServerAdapter.adapt(_).apply(LSPBuilder.create[IO]))
        .map(bindClient(_, clientRef))
    }
    .onFinalizeCase(ec => IO.consoleForIO.errorln("exiting server: " + ec))

  private def bindClient(
    lsp: LSPBuilder[IO],
    clientDef: Deferred[IO, playground.lsp.LanguageClient[IO]],
  ): LSPBuilder[IO] =
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
        clientDef.complete(LangoustineClientAdapter.adapt(communicate)) *>
          lsp.bind(channel, communicate)
    }

}
