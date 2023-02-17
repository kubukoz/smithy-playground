package playground.lsp

import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import cats.effect.std
import playground.TextDocumentManager

/** Entrypoint to the language server. This is later adapted to the lsp4j model and used in Main to
  * launch. Anything workspace-related goes through this instance so this is what we use for some
  * heavier testing too.
  */
object MainServer {

  def makeServer[F[_]: LanguageClient: Async: std.Console]: Resource[F, LanguageServer[F]] =
    TextDocumentManager
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
