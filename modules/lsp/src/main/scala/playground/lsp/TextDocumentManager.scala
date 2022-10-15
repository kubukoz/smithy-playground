package playground

import cats.data.OptionT
import cats.effect.Concurrent
import cats.effect.kernel.Ref
import cats.implicits._
import fs2.io.file.Files
import fs2.io.file.Path

import java.net.URI
import java.nio.file.Paths
import playground.language.TextDocumentProvider

trait TextDocumentManager[F[_]] extends TextDocumentProvider[F] {
  def put(uri: String, text: String): F[Unit]
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

      def getOpt(uri: String): F[Option[String]] = {
        val path = Path.fromNioPath(Paths.get(new URI(uri)))

        Files[F]
          .exists(path)
          .ifM(
            ifTrue = get(uri).map(_.some),
            ifFalse = none[String].pure[F],
          )
      }

      def remove(uri: String): F[Unit] = ref.update(_ - uri)

    }
  }

}
