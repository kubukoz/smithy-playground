package playground.lsp

import cats.data.Chain
import cats.effect.IO
import cats.effect.IOLocal
import cats.implicits._
import cats.~>
import io.circe.Json
import org.eclipse.lsp4j.MessageType

trait TestClient[F[_]] extends LanguageClient[F] {
  def getEvents: F[List[TestClient.Event]]
  def scoped: F ~> F
  def withConfiguration(v: ConfigurationValue.Applied[_]*): F ~> F
}

object TestClient {
  sealed trait Event
  case class MessageLog(tpe: MessageType, msg: String) extends Event
  case object OutputPanelShow extends Event
  case class OutputLog(text: String) extends Event

  final case class State(log: Chain[Event], configuration: Map[String, Json]) {

    def addConfig(values: ConfigurationValue.Applied[_]*): State = copy(configuration =
      configuration ++ values.map { v =>
        v.cv.key -> v.encoded
      }
    )

  }

  def forIO: IO[TestClient[IO]] = IOLocal(
    State(Chain.nil, Map.empty).addConfig(
      ConfigurationValue.authorizationHeader("")
    )
  ).map { state =>
    new TestClient[IO] {
      private def append(events: Event*): IO[Unit] = state.update { s =>
        s.copy(log = s.log.concat(Chain.fromSeq(events)))
      }

      private def show(s: String) = IO.println(Console.GREEN + s + Console.RESET)

      def showOutputPanel: IO[Unit] = show("showing output panel") *> append(OutputPanelShow)

      def logOutput(msg: String): IO[Unit] = show(s"logging output: $msg") *> append(OutputLog(msg))

      def configuration[A](
        v: ConfigurationValue[A]
      ): IO[A] = state
        .get
        .flatMap(_.configuration.get(v.key).liftTo[IO](new Throwable(s"key not found: ${v.key}")))
        .flatMap(_.as[A](v.codec).liftTo[IO])

      def showMessage(tpe: MessageType, msg: String): IO[Unit] =
        show(
          s"${tpe.name()} Message from server: $msg"
        ) *> append(MessageLog(tpe, msg))

      def refreshDiagnostics: IO[Unit] = IO.stub

      def refreshCodeLenses: IO[Unit] = IO.stub

      def getEvents: IO[List[Event]] = state.get.map(_.log.toList)

      def scoped: IO ~> IO =
        new (IO ~> IO) {
          def apply[A](fa: IO[A]): IO[A] = state.getAndReset.bracket(_ => fa)(state.set)
        }

      def withConfiguration(v: ConfigurationValue.Applied[_]*): IO ~> IO =
        new (IO ~> IO) {
          def apply[T](fa: IO[T]): IO[T] =
            state
              .modify { old =>
                (
                  old.addConfig(v: _*),
                  old,
                )
              }
              .bracket(_ => fa)(state.set)
        }
    }
  }

}
