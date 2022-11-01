package playground.language

import cats.Id
import cats.MonadThrow
import cats.implicits._
import playground.CompilationFailed
import playground.CompiledInput
import playground.FileCompiler
import playground.OperationRunner
import playground.smithyql.Query
import playground.smithyql.SourceFile
import playground.smithyql.WithSource
import playground.smithyql.parser.ParsingFailure
import playground.smithyql.parser.SourceParser

import scala.collection.immutable.ListMap
import cats.data.NonEmptyList

trait CommandProvider[F[_]] {
  def runCommand(name: String, args: List[String]): F[Unit]
}

object CommandProvider {

  def instance[F[_]: MonadThrow: TextDocumentProvider: CommandResultReporter](
    compiler: FileCompiler[F],
    runner: OperationRunner.Resolver[F],
  ): CommandProvider[F] =
    new CommandProvider[F] {

      case class RunErrors(issues: NonEmptyList[OperationRunner.Issue]) extends Exception {

        def report: F[Unit] =
          OperationRunner.Issue.squash(issues) match {
            case Left(protocols) => CommandResultReporter[F].onUnsupportedProtocol(protocols)
            case Right(others)   => CommandResultReporter[F].onIssues(others)
          }

      }

      private def runCompiledQuery(
        q: Query[Id],
        input: CompiledInput,
        runner: OperationRunner[F],
      ): F[Unit] = CommandResultReporter[F]
        .onQueryCompiled(q, input)
        .flatMap { requestId =>
          runner
            .run(input)
            .flatMap {
              CommandResultReporter[F].onQuerySuccess(q, requestId, _)
            }
            .handleErrorWith {
              CommandResultReporter[F].onQueryFailure(input, requestId, _)
            }
        }

      private def getRunners(
        file: SourceFile[WithSource]
      ): F[List[OperationRunner[F]]] = file
        .statements
        .map(_.fold(_.query.value))
        .parTraverse(runner.get(_).toEither /* should this toEither be called later? */ )
        .leftMap(RunErrors(_))
        .liftTo[F]

      private def runCompiledFile(
        file: SourceFile[WithSource],
        compiledInputs: List[CompiledInput],
        runners: List[OperationRunner[F]],
      ): F[Unit] =
        CommandResultReporter[F].onFileCompiled *>
          file
            .statements
            .zip(compiledInputs)
            .zip(runners)
            .traverse { case ((stat, input), runner) =>
              stat.fold(
                runQuery =
                  rq =>
                    runCompiledQuery(
                      rq.query.value.mapK(WithSource.unwrap),
                      input,
                      runner,
                    )
              )
            }
            .void

      private def runFile(documentUri: Uri): F[Unit] = {
        for {
          documentText <- TextDocumentProvider[F].get(documentUri)
          file <- SourceParser[SourceFile].parse(documentText).liftTo[F]
          compiledInputs <- compiler.compile(file)
          runners <- getRunners(file)
          _ <- runCompiledFile(file, compiledInputs, runners)
        } yield ()
      }.recoverWith {
        case _: CompilationFailed | _: ParsingFailure =>
          CommandResultReporter[F].onCompilationFailed
        case e: RunErrors => e.report
      }

      private val commandMap: Map[String, List[String] => F[Unit]] = ListMap(
        Command.RUN_QUERY -> {
          case documentUri :: Nil => runFile(Uri.fromUriString(documentUri))
          case s => new Throwable("Unsupported arguments: " + s).raiseError[F, Unit]
        }
      )

      def runCommand(
        name: String,
        args: List[String],
      ): F[Unit] = commandMap
        .get(name)
        .liftTo[F](new Throwable("Unsupported command:  " + name))
        .flatMap(_.apply(args))

    }

}
