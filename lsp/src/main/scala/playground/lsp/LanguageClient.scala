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

trait LanguageClient[F[_]] {
  def configuration(params: List[ConfigurationItem]): F[List[Json]]
}

object LanguageClient {

  def apply[F[_]](implicit F: LanguageClient[F]): LanguageClient[F] = F

  def adapt[F[_]: Async](clientF: F[services.LanguageClient]): LanguageClient[F] =
    new LanguageClient[F] {

      private def withClientF[A](
        f: services.LanguageClient => CompletableFuture[A]
      ): F[A] = clientF.flatMap(cc => Async[F].fromCompletableFuture(Async[F].delay(f(cc))))

      def configuration(
        params: List[ConfigurationItem]
      ): F[List[Json]] = withClientF(_.configuration(new ConfigurationParams(params.asJava)))
        .map(_.asScala.toList.map {
          case e: JsonElement => converters.gsonToCirce(e)
          case e              => throw new RuntimeException(s"Unexpected configuration value: $e")
        })

    }

}
