package playground.lsp.harness

import cats.data.Chain
import cats.effect.IO
import cats.effect.IOLocal
import cats.syntax.all.*
import cats.~>
import io.circe.Json
import playground.lsp.ConfigurationValue
import playground.lsp.LanguageClient
import playground.lsp.MessageType

trait TestClient[F[_]] extends LanguageClient[F] {
  def getEvents: F[List[TestClient.Event]]

  def scoped: F ~> F

  def withConfiguration(
    v: ConfigurationValue.Applied[?]*
  ): F ~> F

}

object TestClient {

  enum Event {

    case MessageLog(
      tpe: MessageType,
      msg: String,
    )

    case OutputPanelShow
    case RefreshCodeLenses
    case RefreshDiagnostics

    case OutputLog(
      text: String
    )

    case CreateWorkDoneProgress(token: String)

    case BeginProgress(
      token: String,
      title: String,
      message: Option[String],
    )

    case ReportProgress(
      token: String,
      message: String,
    )

    case EndProgress(
      token: String,
      message: Option[String],
    )

  }

  final case class State(
    log: Chain[Event],
    configuration: Map[String, Json],
  ) {

    def addConfig(
      values: ConfigurationValue.Applied[?]*
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

      def showOutputPanel: IO[Unit] = show("showing output panel") *> append(Event.OutputPanelShow)

      def logOutput(
        msg: String
      ): IO[Unit] = show(s"logging output: $msg") *> append(Event.OutputLog(msg))

      def configuration[A](
        v: ConfigurationValue[A]
      ): IO[A] = state
        .get
        .flatMap(_.configuration.get(v.key).liftTo[IO](new Throwable(s"key not found: ${v.key}")))
        .flatMap(
          _.as[A](
            using v.codec
          ).liftTo[IO]
        )

      def showMessage(
        tpe: MessageType,
        msg: String,
      ): IO[Unit] = {
        val color =
          tpe match {
            case MessageType.Error   => Console.MAGENTA
            case MessageType.Warning => Console.YELLOW
            case MessageType.Info    => Console.GREEN
          }

        show(
          s = s"${tpe.name.toUpperCase()} Message from server: $msg",
          color = color,
        ) *> append(Event.MessageLog(tpe, msg))
      }
      def refreshDiagnostics: IO[Unit] =
        show("Refreshing diagnostics...") *> append(Event.RefreshDiagnostics)

      def refreshCodeLenses: IO[Unit] =
        show("Refreshing code lenses...") *> append(Event.RefreshCodeLenses)

      def getEvents: IO[List[Event]] = state.get.map(_.log.toList)

      /* Clears the state for the duration of the task, then restores it */
      def scoped: IO ~> IO =
        new (IO ~> IO) {
          def apply[A](
            fa: IO[A]
          ): IO[A] = state.getAndReset.bracket(_ => fa)(state.set)
        }

      def withConfiguration(
        v: ConfigurationValue.Applied[?]*
      ): IO ~> IO =
        new (IO ~> IO) {
          def apply[T](
            fa: IO[T]
          ): IO[T] =
            state
              .modify { old =>
                (
                  old.addConfig(v*),
                  old,
                )
              }
              .bracket(_ => fa)(state.set)
        }

      def enableProgressCapability: IO[Unit] = IO.unit
      def hasProgressCapability: IO[Boolean] = IO.pure(true)
      def createWorkDoneProgress(token: String): IO[Unit] = append(
        Event.CreateWorkDoneProgress(token)
      )

      def beginProgress(token: String, title: String, message: Option[String]): IO[Unit] = append(
        Event.BeginProgress(token, title, message)
      )

      def endProgress(token: String, message: Option[String]): IO[Unit] = append(
        Event.EndProgress(token, message)
      )

      def reportProgress(token: String, message: Option[String]): IO[Unit] = append(
        Event.ReportProgress(token, message.getOrElse(""))
      )
    }
  }

}
