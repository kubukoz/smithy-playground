package playground.lsp

import org.eclipse.lsp4j.ConfigurationParams
import org.eclipse.lsp4j.services
import cats.effect.kernel.Async
import java.util.concurrent.CompletableFuture
import cats.implicits._
import com.google.gson.JsonElement

import scala.jdk.CollectionConverters._
import org.eclipse.lsp4j.ConfigurationItem
import playground.Feedback
import org.eclipse.lsp4j.MessageParams
import org.eclipse.lsp4j.MessageType
import scala.util.chaining._
import io.circe.Decoder

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

      def showErrorMessage(msg: String): F[Unit] = withClientSync(
        _.showMessage(new MessageParams(MessageType.Error, msg))
      )

      def logOutput(msg: String): F[Unit] = withClientSync(
        _.logMessage(new MessageParams().tap(_.setMessage(msg)))
      )

      def showOutputPanel: F[Unit] = withClientSync(_.showOutputPanel())

    }

  def suspend[F[_]: Async](clientF: F[LanguageClient[F]]): LanguageClient[F] =
    new LanguageClient[F] {

      def showErrorMessage(msg: String): F[Unit] = clientF.flatMap(_.showErrorMessage(msg))

      def logOutput(msg: String): F[Unit] = clientF.flatMap(_.logOutput(msg))

      def configuration[A: Decoder](
        section: String
      ): F[A] = clientF.flatMap(_.configuration(section))

      val showOutputPanel: F[Unit] = clientF.flatMap(_.showOutputPanel)

    }

}
