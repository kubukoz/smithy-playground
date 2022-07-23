package playground.lsp

import org.eclipse.lsp4j.ConfigurationParams
import io.circe.Json
import org.eclipse.lsp4j.services
import cats.effect.kernel.Async
import java.util.concurrent.CompletableFuture
import cats.implicits._
import com.google.gson.JsonElement

import scala.jdk.CollectionConverters._
import io.circe.JsonNumber
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
          case e: JsonElement => gsonToCirce(e)
          case e              => throw new RuntimeException(s"Unexpected configuration value: $e")
        })

      private def gsonToCirce(gson: JsonElement): Json =
        if (gson.isJsonPrimitive()) {
          val prim = gson.getAsJsonPrimitive()

          if (prim.isString())
            Json.fromString(prim.getAsString())
          else if (prim.isNumber())
            Json.fromJsonNumber(JsonNumber.fromString(prim.getAsString()).get)
          else if (prim.isBoolean())
            Json.fromBoolean(prim.getAsBoolean())
          else
            throw new IllegalArgumentException(s"Unknown primitive: $prim")
        } else if (gson.isJsonArray()) {
          Json.fromValues(gson.getAsJsonArray().asScala.map(gsonToCirce).toList)
        } else if (gson.isJsonObject()) {
          Json.fromFields(
            gson
              .getAsJsonObject()
              .entrySet()
              .asScala
              .map { case entry =>
                val key = entry.getKey
                val value = gsonToCirce(entry.getValue)
                key -> value
              }
              .toList
          )
        } else
          Json.Null

    }

}
