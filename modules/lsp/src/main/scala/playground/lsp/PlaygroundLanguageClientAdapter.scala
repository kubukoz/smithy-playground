package playground.lsp

import cats.effect.kernel.Async
import cats.effect.kernel.Ref
import cats.syntax.all.*
import com.google.gson.JsonElement
import org.eclipse.lsp4j
import org.eclipse.lsp4j.ProgressParams
import org.eclipse.lsp4j.WorkDoneProgressBegin
import org.eclipse.lsp4j.WorkDoneProgressCreateParams
import org.eclipse.lsp4j.WorkDoneProgressEnd
import org.eclipse.lsp4j.WorkDoneProgressNotification
import org.eclipse.lsp4j.WorkDoneProgressReport
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification

import java.util.concurrent.CompletableFuture
import scala.jdk.CollectionConverters.*
import scala.util.chaining.*

object PlaygroundLanguageClientAdapter {

  def adapt[F[_]: Async](
    client: PlaygroundLanguageClient
  ): F[LanguageClient[F]] = Ref[F].of(false).map(adaptInternal(client, _))

  private def adaptInternal[F[_]: Async](
    client: PlaygroundLanguageClient,
    progressCapabilityState: Ref[F, Boolean],
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
        .flatMap(
          _.as[A](
            using v.codec
          ).liftTo[F]
        )

      def enableProgressCapability: F[Unit] = progressCapabilityState.set(true)
      def hasProgressCapability: F[Boolean] = progressCapabilityState.get

      def createWorkDoneProgress(token: String): F[Unit] =
        withClientF(
          _.createProgress(
            new WorkDoneProgressCreateParams(
              converters.toLSP.either(token.asLeft)
            )
          )
        ).void

      def beginProgress(token: String, title: String, message: Option[String]): F[Unit] = progress(
        token,
        new WorkDoneProgressBegin()
          .tap(_.setTitle(title))
          .tap(_.setMessage(message.orNull)),
      )

      def reportProgress(token: String, message: Option[String]): F[Unit] = progress(
        token,
        new WorkDoneProgressReport()
          .tap(_.setMessage(message.orNull)),
      )

      def endProgress(token: String, message: Option[String]): F[Unit] = progress(
        token,
        new WorkDoneProgressEnd().tap(_.setMessage(message.orNull)),
      )

      private def progress(token: String, progress: WorkDoneProgressNotification) = withClientSync(
        _.notifyProgress(
          new ProgressParams(
            converters.toLSP.either(Left(token)),
            converters
              .toLSP
              .either(progress.asLeft),
          )
        )
      )

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
