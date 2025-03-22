package playground.lsp

import cats.effect.kernel.Async
import cats.syntax.all.*
import com.google.gson.JsonElement
import org.eclipse.lsp4j
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification

import java.util.concurrent.CompletableFuture
import scala.jdk.CollectionConverters.*
import scala.util.chaining.*

object PlaygroundLanguageClientAdapter {

  def adapt[F[_]: Async](
    client: PlaygroundLanguageClient
  ): LanguageClient[F] =
    new LanguageClient[F] {

      private def withClientF[A](
        f: client.type => CompletableFuture[A]
      ): F[A] = Async[F].fromCompletableFuture(Async[F].delay(f(client)))

      private def withClientSync[A](
        f: client.type => A
      ): F[A] = Async[F].delay(f(client))

      def configuration[A](
        v: ConfigurationValue[A]
      ): F[A] = withClientF(
        _.configuration(
          new lsp4j.ConfigurationParams(
            (new lsp4j.ConfigurationItem().tap(_.setSection(v.key)) :: Nil).asJava
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
        .flatMap(_.as[A](v.codec).liftTo[F])

      def showMessage(
        tpe: MessageType,
        msg: String,
      ): F[Unit] = withClientSync(
        _.showMessage(new lsp4j.MessageParams(converters.toLSP.messageType(tpe), msg))
      )

      def logOutput(
        msg: String
      ): F[Unit] = withClientSync(
        _.logMessage(new lsp4j.MessageParams().tap(_.setMessage(msg)))
      )

      def showOutputPanel: F[Unit] = withClientSync(_.showOutputPanel())

      def refreshCodeLenses: F[Unit] = withClientF(_.refreshCodeLenses()).void
      def refreshDiagnostics: F[Unit] = withClientF(_.refreshDiagnostics()).void
    }

}

trait PlaygroundLanguageClient extends lsp4j.services.LanguageClient {

  @JsonNotification("smithyql/showOutputPanel")
  def showOutputPanel(
  ): Unit

}
