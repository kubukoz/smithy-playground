package playground.lsp

import org.eclipse.lsp4j.ConfigurationParams
import io.circe.Json
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

trait LanguageClient[F[_]] extends Feedback[F] {
  def configuration(params: List[ConfigurationItem]): F[List[Json]]
}

object LanguageClient {

  def apply[F[_]](implicit F: LanguageClient[F]): LanguageClient[F] = F

  def adapt[F[_]: Async](client: services.LanguageClient): LanguageClient[F] =
    new LanguageClient[F] {

      private def withClientF[A](
        f: services.LanguageClient => CompletableFuture[A]
      ): F[A] = Async[F].fromCompletableFuture(Async[F].delay(f(client)))

      private def withClientSync[A](
        f: services.LanguageClient => A
      ): F[A] = Async[F].delay(f(client))

      def configuration(
        params: List[ConfigurationItem]
      ): F[List[Json]] = withClientF(_.configuration(new ConfigurationParams(params.asJava)))
        .map(_.asScala.toList.map {
          case e: JsonElement => converters.gsonToCirce(e)
          case e              => throw new RuntimeException(s"Unexpected configuration value: $e")
        })

      def showErrorMessage(msg: String): F[Unit] = withClientSync(
        _.showMessage(new MessageParams(MessageType.Error, msg))
      )

      def logOutput(msg: String): F[Unit] = withClientSync(
        _.logMessage(new MessageParams().tap(_.setMessage(msg)))
      )

    }

  def suspend[F[_]: Async](clientF: F[LanguageClient[F]]): LanguageClient[F] =
    new LanguageClient[F] {

      def showErrorMessage(msg: String): F[Unit] = clientF.flatMap(_.showErrorMessage(msg))

      def logOutput(msg: String): F[Unit] = clientF.flatMap(_.logOutput(msg))

      def configuration(
        params: List[ConfigurationItem]
      ): F[List[Json]] = clientF.flatMap(_.configuration(params))

    }

}
