package playground.lsp

import cats.data.Chain
import cats.effect.IO
import io.circe.Decoder
import org.eclipse.lsp4j.MessageType
import cats.effect.IOLocal

trait TestClient[F[_]] extends LanguageClient[F] {
  def getEvents: F[List[TestClient.Event]]
  def withClearEvents[A](f: F[A]): F[A]
}

object TestClient {
  sealed trait Event
  case class MessageLog(tpe: MessageType, msg: String) extends Event
  case object OutputPanelShow extends Event
  case class OutputLog(text: String) extends Event

  def forIO: IO[TestClient[IO]] = IOLocal(Chain.empty[Event]).map { log =>
    new TestClient[IO] {
      private def append(events: Event*): IO[Unit] = log.update(_.concat(Chain.fromSeq(events)))

      private def show(s: String) = IO.println(Console.GREEN + s + Console.RESET)

      def showOutputPanel: IO[Unit] = show("showing output panel") *> append(OutputPanelShow)

      def logOutput(msg: String): IO[Unit] = show(s"logging output: $msg") *> append(OutputLog(msg))

      def configuration[A: Decoder](section: String): IO[A] = IO.stub

      def showMessage(tpe: MessageType, msg: String): IO[Unit] =
        show(
          s"${tpe.name()} Message from server: $msg"
        ) *> append(MessageLog(tpe, msg))

      def refreshDiagnostics: IO[Unit] = IO.stub

      def refreshCodeLenses: IO[Unit] = IO.stub

      def getEvents: IO[List[Event]] = log.get.map(_.toList)

      def withClearEvents[A](
        f: IO[A]
      ): IO[A] = log.getAndReset.bracket(_ => f)(log.set)
    }
  }

}
