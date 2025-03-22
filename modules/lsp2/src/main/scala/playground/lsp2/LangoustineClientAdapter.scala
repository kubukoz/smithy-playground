package playground.lsp2

import cats.Functor
import cats.syntax.all.*
import langoustine.lsp.Communicate
import langoustine.lsp.enumerations.MessageType
import langoustine.lsp.requests.CustomNotification
import langoustine.lsp.requests.window
import langoustine.lsp.requests.workspace
import langoustine.lsp.structures.LogMessageParams
import langoustine.lsp.structures.ShowMessageParams

object LangoustineClientAdapter {

  def adapt[F[_]: Functor](comms: Communicate[F]): playground.lsp.LanguageClient[F] =
    new {
      def logOutput(msg: String): F[Unit] = comms.notification(
        window.logMessage(LogMessageParams(`type` = MessageType.Info, message = msg))
      )

      def configuration[A](v: playground.lsp.ConfigurationValue[A]): F[A] = ??? // for later

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
    }

  object smithyql {
    object showOutputPanel extends CustomNotification[Unit]("smithyql/showOutputPanel")
  }

}
