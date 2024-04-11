package playground

import cats.data.OptionT
import cats.effect.Concurrent
import cats.effect.kernel.Ref
import cats.syntax.all.*
import fs2.io.file.Files
import playground.language.TextDocumentProvider
import playground.language.Uri

trait TextDocumentManager[F[_]] extends TextDocumentProvider[F] {

  def put(
    uri: Uri,
    text: String,
  ): F[Unit]

  def remove(
    uri: Uri
  ): F[Unit]

}

object TextDocumentManager {

  def apply[F[_]](
    implicit F: TextDocumentManager[F]
  ): TextDocumentManager[F] = F

  def instance[F[_]: Files: Concurrent]: F[TextDocumentManager[F]] = Ref[F]
    .of(Map.empty[Uri, String])
    .map { ref =>
      new TextDocumentManager[F] {

        def put(
          uri: Uri,
          text: String,
        ): F[Unit] = ref.update(_ + (uri -> text))

        def get(
          uri: Uri
        ): F[String] = OptionT(ref.get.map(_.get(uri))).getOrElseF {
          Files[F]
            .readAll(uri.toPath)
            .through(fs2.text.utf8.decode[F])
            .compile
            .string
        }

        def getOpt(
          uri: Uri
        ): F[Option[String]] = Files[F]
          .exists(uri.toPath)
          .ifM(
            ifTrue = get(uri).map(_.some),
            ifFalse = none[String].pure[F],
          )

        def remove(
          uri: Uri
        ): F[Unit] = ref.update(_ - uri)

      }
    }

}
