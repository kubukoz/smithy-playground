package playground.lsp

import cats.effect.kernel.Async
import cats.implicits._
import com.google.gson.JsonElement
import io.circe.Decoder
import org.eclipse.lsp4j.ConfigurationItem
import org.eclipse.lsp4j.ConfigurationParams
import org.eclipse.lsp4j.MessageParams
import org.eclipse.lsp4j.MessageType
import playground.Feedback

import java.util.concurrent.CompletableFuture
import scala.jdk.CollectionConverters._
import scala.util.chaining._
import cats.tagless.SuspendK
import cats.~>
import cats.tagless.Derive
import cats.tagless.SuspendK
import cats.tagless.Suspended

trait LanguageClient[F[_]] extends Feedback[F] {
  def configuration[A: Decoder](section: String): F[A]
}

object LanguageClient {

  def apply[F[_]](implicit F: LanguageClient[F]): LanguageClient[F] = F

  def adapt[F[_]: Async](client: PlaygroundLanguageClient): LanguageClient[F] =
    new LanguageClient[F] {

      private def withClientF[A](
        f: client.type => CompletableFuture[A]
      ): F[A] = Async[F].fromCompletableFuture(Async[F].delay(f(client)))

      private def withClientSync[A](
        f: client.type => A
      ): F[A] = Async[F].delay(f(client))

      def configuration[A: Decoder](
        section: String
      ): F[A] = withClientF(
        _.configuration(
          new ConfigurationParams(
            (new ConfigurationItem().tap(_.setSection(section)) :: Nil).asJava
          )
        )
      )
        .flatMap {
          _.asScala
            .toList
            .headOption
            .liftTo[F](new Throwable("missing entry in the response"))
        }
        .map {
          case e: JsonElement => converters.gsonToCirce(e)
          case e              => throw new RuntimeException(s"Unexpected configuration value: $e")
        }
        .flatMap(_.as[A].liftTo[F])

      private def showMessage(tpe: MessageType, msg: String): F[Unit] = withClientSync(
        _.showMessage(new MessageParams(tpe, msg))
      )

      def showInfoMessage(msg: String): F[Unit] = showMessage(MessageType.Info, msg)

      def showErrorMessage(msg: String): F[Unit] = showMessage(MessageType.Error, msg)

      def logOutput(msg: String): F[Unit] = withClientSync(
        _.logMessage(new MessageParams().tap(_.setMessage(msg)))
      )

      def showOutputPanel: F[Unit] = withClientSync(_.showOutputPanel())

    }

  implicit val suspendKClient: SuspendK[LanguageClient] =
    new SuspendK[LanguageClient] {
      private val funk = Derive.functorK[LanguageClient]

      def mapK[F[_], G[_]](af: LanguageClient[F])(fk: F ~> G): LanguageClient[G] = funk.mapK(af)(fk)

      def suspend[F[_]]: LanguageClient[Suspended[LanguageClient, F, *]] =
        new LanguageClient[Suspended[LanguageClient, F, *]] {

          def showInfoMessage(msg: String): Suspended[LanguageClient, F, Unit] = Suspended(
            _.showInfoMessage(msg)
          )

          def showErrorMessage(msg: String): Suspended[LanguageClient, F, Unit] = Suspended(
            _.showErrorMessage(msg)
          )

          def showOutputPanel: Suspended[LanguageClient, F, Unit] = Suspended(_.showOutputPanel)

          def logOutput(msg: String): Suspended[LanguageClient, F, Unit] = Suspended(
            _.logOutput(msg)
          )

          def configuration[A: Decoder](section: String): Suspended[LanguageClient, F, A] =
            Suspended(_.configuration(section))
        }

    }

  def suspend[F[_]: Async](clientF: F[LanguageClient[F]]): LanguageClient[F] =
    SuspendK[LanguageClient].deferKId(clientF)

}
