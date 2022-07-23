package playground.lsp

import cats.effect.kernel.Ref
import fs2.io.file.Files
import cats.implicits._
import cats.effect.Concurrent
import cats.data.OptionT
import fs2.io.file.Path
import java.nio.file.Paths
import java.net.URI

trait TextDocumentManager[F[_]] {
  def put(uri: String, text: String): F[Unit]
  def get(uri: String): F[String]
  def remove(uri: String): F[Unit]
}

object TextDocumentManager {

  def apply[F[_]](implicit F: TextDocumentManager[F]): TextDocumentManager[F] = F

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
