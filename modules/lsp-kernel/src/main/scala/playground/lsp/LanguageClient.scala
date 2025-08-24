package playground.lsp

import cats.FlatMap
import cats.data.Kleisli
import cats.syntax.all.*
import cats.tagless.Derive
import cats.tagless.FunctorK
import cats.tagless.implicits.*
import cats.~>
import playground.language.Feedback

trait LanguageClient[F[_]] extends Feedback[F] {

  def configuration[A](
    v: ConfigurationValue[A]
  ): F[A]

  def showMessage(
    tpe: MessageType,
    msg: String,
  ): F[Unit]

  def refreshDiagnostics: F[Unit]
  def refreshCodeLenses: F[Unit]

  def showInfoMessage(
    msg: String
  ): F[Unit] = showMessage(MessageType.Info, msg)

  def showWarnMessage(
    msg: String
  ): F[Unit] = showMessage(MessageType.Warning, msg)

  def showErrorMessage(
    msg: String
  ): F[Unit] = showMessage(MessageType.Error, msg)

  def enableProgressCapability: F[Unit]
}

object LanguageClient {

  def apply[F[_]](
    implicit F: LanguageClient[F]
  ): LanguageClient[F] = F

  implicit val functorK: FunctorK[LanguageClient] = Derive.functorK[LanguageClient]

  def defer[F[_]: FlatMap](
    fa: F[LanguageClient[F]]
  ): LanguageClient[F] = Derive
    .readerT[LanguageClient, F]
    .mapK(new (Kleisli[F, LanguageClient[F], *] ~> F) {

      def apply[A](
        k: Kleisli[F, LanguageClient[F], A]
      ): F[A] = fa.flatMap(k.run)

    })

}

enum MessageType {
  case Info, Warning, Error

  def name: String = toString
}
