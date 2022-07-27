package playground.lsp

import cats.data.OptionT
import cats.effect.Concurrent
import cats.effect.kernel.Ref
import cats.implicits._
import fs2.io.file.Files
import fs2.io.file.Path
import playground.TextDocumentManager
import java.net.URI
import java.nio.file.Paths

object WorkspaceTextDocumentManager {

  def instance[
    F[_]: Files: Concurrent
  ]: F[TextDocumentManager[F]] = Ref[F].of(Map.empty[String, String]).map { ref =>
    new TextDocumentManager[F] {
      def put(uri: String, text: String): F[Unit] = ref.update(_ + (uri -> text))

      def get(uri: String): F[String] = OptionT(ref.get.map(_.get(uri))).getOrElseF {
        Files[F]
          .readAll(Path.fromNioPath(Paths.get(new URI(uri))))
          .through(fs2.text.utf8.decode[F])
          .compile
          .string
      }

      def remove(uri: String): F[Unit] = ref.update(_ - uri)

    }
  }

}
