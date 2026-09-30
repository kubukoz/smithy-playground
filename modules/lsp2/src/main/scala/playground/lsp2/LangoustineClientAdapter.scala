package playground.lsp2

import cats.effect.kernel.Ref
import cats.effect.kernel.Sync
import cats.syntax.all.*
import langoustine.lsp.Communicate
import langoustine.lsp.aliases.ProgressToken
import langoustine.lsp.enumerations.MessageType
import langoustine.lsp.requests.window
import langoustine.lsp.requests.workspace
import langoustine.lsp.runtime.Opt
import langoustine.lsp.structures.ConfigurationItem
import langoustine.lsp.structures.ConfigurationParams
import langoustine.lsp.structures.LogMessageParams
import langoustine.lsp.structures.ProgressParams
import langoustine.lsp.structures.ShowMessageParams
import langoustine.lsp.structures.WorkDoneProgressBegin
import langoustine.lsp.structures.WorkDoneProgressCreateParams
import langoustine.lsp.structures.WorkDoneProgressEnd
import langoustine.lsp.structures.WorkDoneProgressReport
import playground.lsp2.LangoustineServerAdapter.converters

import ProtocolExtensions.smithyql

object LangoustineClientAdapter {

  def adapt[F[_]: Sync](comms: Communicate[F]): F[playground.lsp.LanguageClient[F]] = Ref[F]
    .of(false)
    .map(adaptInternal(comms, _))

  private def adaptInternal[F[_]: Sync](
    comms: Communicate[F],
    progressCapabilityState: Ref[F, Boolean],
  ): playground.lsp.LanguageClient[F] =
    new {
      def logOutput(msg: String): F[Unit] = comms.notification(
        window.logMessage(LogMessageParams(`type` = MessageType.Info, message = msg))
      )

      def configuration[A](v: playground.lsp.ConfigurationValue[A]): F[A] = comms
        .request(
          workspace.configuration(
            ConfigurationParams(
              Vector(
                ConfigurationItem(
                  section = Opt(v.key)
                )
              )
            )
          )
        )
        .flatMap(_.headOption.liftTo[F](new Throwable("missing entry in the response")))
        .map(converters.fromLSP.json)
        .flatMap(
          _.as[A](
            using v.codec
          ).liftTo[F]
        )

      def refreshCodeLenses: F[Unit] = comms.request(workspace.codeLens.refresh(())).void
      def refreshDiagnostics: F[Unit] = comms.request(workspace.diagnostic.refresh(())).void

      def showMessage(tpe: playground.lsp.MessageType, msg: String): F[Unit] = comms.notification(
        window.showMessage(
          ShowMessageParams(
            `type` =
              tpe match {
                case playground.lsp.MessageType.Error   => MessageType.Error
                case playground.lsp.MessageType.Info    => MessageType.Info
                case playground.lsp.MessageType.Warning => MessageType.Warning
              },
            message = msg,
          )
        )
      )

      def showOutputPanel: F[Unit] = comms.notification(smithyql.showOutputPanel(()))

      def enableProgressCapability: F[Unit] = progressCapabilityState.set(true)
      def hasProgressCapability: F[Boolean] = progressCapabilityState.get

      def createWorkDoneProgress(token: String): F[Unit] =
        comms
          .request(
            window
              .workDoneProgress
              .create(WorkDoneProgressCreateParams(token = ProgressToken(token)))
          )
          .void

      def beginProgress(token: String, title: String, message: Option[String]): F[Unit] = progress(
        token,
        upickle
          .default
          .writeJs(
            WorkDoneProgressBegin(kind = "begin", title = title, message = Opt.fromOption(message))
          ),
      )

      def reportProgress(token: String, message: Option[String]): F[Unit] = progress(
        token,
        upickle
          .default
          .writeJs(WorkDoneProgressReport(kind = "report", message = Opt.fromOption(message))),
      )

      def endProgress(token: String, message: Option[String]): F[Unit] = progress(
        token,
        upickle
          .default
          .writeJs(WorkDoneProgressEnd(kind = "end", message = Opt.fromOption(message))),
      )

      private def progress(token: String, value: ujson.Value): F[Unit] = comms.notification(
        langoustine
          .lsp
          .requests
          .$DOLLAR
          .progress(ProgressParams(token = ProgressToken(token), value = value))
      )
    }

}
