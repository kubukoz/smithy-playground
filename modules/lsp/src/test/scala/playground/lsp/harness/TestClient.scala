package playground.lsp.harness

import cats.data.Chain
import cats.effect.IO
import cats.effect.IOLocal
import cats.implicits._
import cats.~>
import io.circe.Json
import org.eclipse.lsp4j.MessageType
import playground.lsp.ConfigurationValue
import playground.lsp.LanguageClient

trait TestClient[F[_]] extends LanguageClient[F] {
  def getEvents: F[List[TestClient.Event]]
  def scoped: F ~> F

  def withConfiguration(
    v: ConfigurationValue.Applied[_]*
  ): F ~> F

}

object TestClient {
  sealed trait Event

  case class MessageLog(
    tpe: MessageType,
    msg: String,
  ) extends Event

  case object OutputPanelShow extends Event
  case object RefreshCodeLenses extends Event
  case object RefreshDiagnostics extends Event

  case class OutputLog(
    text: String
  ) extends Event

  final case class State(
    log: Chain[Event],
    configuration: Map[String, Json],
  ) {

    def addConfig(
      values: ConfigurationValue.Applied[_]*
    ): State = copy(configuration =
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
      private def append(
        events: Event*
      ): IO[Unit] = state.update { s =>
        s.copy(log = s.log.concat(Chain.fromSeq(events)))
      }

      private def show(
        s: String,
        color: String = "",
      ) = IO.println(color + s + Console.RESET)

      def showOutputPanel: IO[Unit] = show("showing output panel") *> append(OutputPanelShow)

      def logOutput(
        msg: String
      ): IO[Unit] = show(s"logging output: $msg") *> append(OutputLog(msg))

      def configuration[A](
        v: ConfigurationValue[A]
      ): IO[A] = state
        .get
        .flatMap(_.configuration.get(v.key).liftTo[IO](new Throwable(s"key not found: ${v.key}")))
        .flatMap(_.as[A](v.codec).liftTo[IO])

      def showMessage(
        tpe: MessageType,
        msg: String,
      ): IO[Unit] = {
        val color =
          tpe match {
            case MessageType.Error   => Console.MAGENTA
            case MessageType.Warning => Console.YELLOW
            case MessageType.Info    => Console.GREEN
            case MessageType.Log     => ""
          }

        show(
          s = s"${tpe.name().toUpperCase()} Message from server: $msg",
          color = color,
        ) *> append(MessageLog(tpe, msg))
      }
      def refreshDiagnostics: IO[Unit] =
        show("Refreshing diagnostics...") *> append(RefreshDiagnostics)

      def refreshCodeLenses: IO[Unit] =
        show("Refreshing code lenses...") *> append(RefreshCodeLenses)

      def getEvents: IO[List[Event]] = state.get.map(_.log.toList)

      def scoped: IO ~> IO =
        new (IO ~> IO) {
          def apply[A](
            fa: IO[A]
          ): IO[A] = state.getAndReset.bracket(_ => fa)(state.set)
        }

      def withConfiguration(
        v: ConfigurationValue.Applied[_]*
      ): IO ~> IO =
        new (IO ~> IO) {
          def apply[T](
            fa: IO[T]
          ): IO[T] =
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
