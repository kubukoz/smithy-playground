package playground.lsp

import cats.effect.implicits.*
import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import cats.effect.std
import cats.effect.std.UUIDGen
import fs2.compression.Compression
import fs2.io.file.Files
import fs2.io.net.Network

/** Entrypoint to the language server. This is later adapted to the lsp framework's model and used
  * in Main to launch. Anything workspace-related goes through this instance so this is what we use
  * for some heavier testing too.
  */
object MainServer {

  def makeServer[F[_]: LanguageClient: Async: Files: Network: Compression: std.Console: UUIDGen]
    : Resource[F, LanguageServer[F]] = TextDocumentManager
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
