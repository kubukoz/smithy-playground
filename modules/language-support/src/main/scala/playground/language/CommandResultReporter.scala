package playground.language

import cats.Id
import cats.Monad
import cats.data.NonEmptyList
import cats.effect.kernel.Ref
import cats.implicits._
import playground.CompiledInput
import playground.smithyql.InputNode
import playground.smithyql.Query
import playground.smithyql.WithSource
import playground.smithyql.format.Formatter

trait CommandResultReporter[F[_]] {
  type RequestId
  def onUnsupportedProtocol: F[Unit]
  def onIssues(issues: NonEmptyList[Throwable]): F[Unit]
  def onCompilationFailed: F[Unit]
  def onFileCompiled(queries: List[Any]): F[Unit]
  def onQueryStart(parsed: Query[Id], compiled: CompiledInput): F[RequestId]
  def onQuerySuccess(parsed: Query[Id], requestId: RequestId, output: InputNode[Id]): F[Unit]
  def onQueryFailure(compiled: CompiledInput, requestId: RequestId, e: Throwable): F[Unit]
}

object CommandResultReporter {
  def apply[F[_]](implicit F: CommandResultReporter[F]): F.type = F

  def instance[
    F[_]: Feedback: Monad: Ref.Make
  ]: F[CommandResultReporter[F]] = Ref[F].of(0).map(withRequestCounter(_))

  def withRequestCounter[F[_]: Feedback: Monad](
    requestCounter: Ref[F, Int]
  ): CommandResultReporter[F] =
    new CommandResultReporter[F] {

      type RequestId = Int

      def onUnsupportedProtocol: F[Unit] = Feedback[F].showErrorMessage(
        """At least 1 service in the file uses an unsupported protocol.
          |Check diagnostics/problems in the file.""".stripMargin
      )

      def onIssues(issues: NonEmptyList[Throwable]): F[Unit] = Feedback[F].showErrorMessage(
        issues.map(_.toString).mkString_("\n\n")
      )

      def onCompilationFailed: F[Unit] = Feedback[F].showErrorMessage(
        "Couldn't run query because of compilation errors."
      )

      def onFileCompiled(queries: List[Any]): F[Unit] =
        if (queries.nonEmpty)
          Feedback[F].showOutputPanel
        else
          Feedback[F].showWarnMessage("No operations to run in file")

      def onQueryStart(
        parsed: Query[Id],
        compiled: CompiledInput,
      ): F[RequestId] = requestCounter.updateAndGet(_ + 1).flatTap { requestId =>
        Feedback[F]
          .logOutput(
            s"// Calling ${parsed.operationName.operationName.text} ($requestId)"
          )
      }

      def onQuerySuccess(parsed: Query[Id], requestId: RequestId, out: InputNode[Id]): F[Unit] =
        Feedback[F].logOutput(
          s"// Succeeded ${parsed.operationName.operationName.text} ($requestId), response:\n"
            + writeOutput(out)
        )

      def onQueryFailure(compiled: CompiledInput, requestId: Int, e: Throwable): F[Unit] = {
        val rendered =
          compiled
            .catchError(e)
            .flatMap(err => compiled.writeError.map(_.toNode(err))) match {
            case Some(e) => "\n" + writeOutput(e)
            case None    => e.toString
          }

        Feedback[F].logOutput(s"// ERROR ($requestId) $rendered")
      }

      private def writeOutput(
        node: InputNode[cats.Id]
      ) = Formatter.inputNodeFormatter.format(node.mapK(WithSource.liftId), 80)

    }

}
