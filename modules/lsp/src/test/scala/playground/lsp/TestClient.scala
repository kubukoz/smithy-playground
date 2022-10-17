package playground.lsp

import cats.data.Chain
import cats.effect.IO
import io.circe.Decoder
import org.eclipse.lsp4j.MessageType

trait TestClient[F[_]] extends LanguageClient[F] {
  def getLogs: F[List[TestClient.MessageLog]]
}

object TestClient {
  case class MessageLog(tpe: MessageType, msg: String)

  def forIO: IO[TestClient[IO]] = IO.ref(Chain.empty[MessageLog]).map { log =>
    new TestClient[IO] {
      def showOutputPanel: IO[Unit] = IO.stub

      def logOutput(msg: String): IO[Unit] = IO.stub

      def configuration[A: Decoder](section: String): IO[A] = IO.stub

      def showMessage(tpe: MessageType, msg: String): IO[Unit] =
        IO.println(
          s"${tpe.name()} Message from server: $msg"
        ) *> log.update(_.append(MessageLog(tpe, msg)))

      def refreshDiagnostics: IO[Unit] = IO.stub

      def refreshCodeLenses: IO[Unit] = IO.stub

      def getLogs: IO[List[MessageLog]] = log.get.map(_.toList)
    }
  }

}
