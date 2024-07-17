package playground.language

import cats.Applicative
import cats.syntax.all.*

trait TextDocumentProvider[F[_]] {

  def get(
    uri: Uri
  ): F[String]

  def getOpt(
    uri: Uri
  ): F[Option[String]]

}

object TextDocumentProvider {

  def apply[F[_]](
    implicit F: TextDocumentProvider[F]
  ): TextDocumentProvider[F] = F

  def always[F[_]: Applicative](
    fileContent: String
  ): TextDocumentProvider[F] =
    new TextDocumentProvider[F] {

      def get(
        uri: Uri
      ): F[String] = fileContent.pure[F]

      def getOpt(
        uri: Uri
      ): F[Option[String]] = fileContent.some.pure[F]

    }

}
