package playground.language

import cats.Id
import cats.MonadThrow
import cats.data.NonEmptyList
import cats.implicits._
import playground.CompilationFailed
import playground.CompiledInput
import playground.FileCompiler
import playground.FileRunner
import playground.OperationRunner
import playground.smithyql.Query
import playground.smithyql.SourceFile
import playground.smithyql.WithSource
import playground.smithyql.parser.ParsingFailure
import playground.smithyql.parser.SourceParser

import scala.collection.immutable.ListMap

trait CommandProvider[F[_]] {
  def runCommand(name: String, args: List[String]): F[Unit]
}

object CommandProvider {

  def instance[F[_]: MonadThrow: TextDocumentProvider: CommandResultReporter](
    compiler: FileCompiler[F],
    runner: FileRunner.Resolver[F],
  ): CommandProvider[F] =
    new CommandProvider[F] {

      case class RunErrors(issues: NonEmptyList[OperationRunner.Issue.Squashed]) extends Exception {

        def report: F[Unit] = {
          val (protocolIssues, otherIssues) = issues.toList.partitionMap {
            case p: OperationRunner.Issue.Squashed.ProtocolIssues => p.asLeft

            case p: OperationRunner.Issue.Squashed.OtherIssues => p.asRight
          }

          CommandResultReporter[F]
            .onUnsupportedProtocol
            .whenA(protocolIssues.nonEmpty) *>
            otherIssues.traverse_ { case OperationRunner.Issue.Squashed.OtherIssues(others) =>
              CommandResultReporter[F].onIssues(others)
            }
        }

      }

      private def runCompiledQuery(
        q: Query[Id],
        input: CompiledInput,
        runner: OperationRunner[F],
      ): F[Unit] = CommandResultReporter[F]
        .onQueryStart(q, input)
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

      private def runCompiledFile(
        file: SourceFile[WithSource],
        compiledInputs: List[CompiledInput],
        runners: List[OperationRunner[F]],
      ): F[Unit] = {
        val queries = file.queries(WithSource.unwrap)

        CommandResultReporter[F].onFileCompiled(queries) *>
          queries
            .zip(compiledInputs)
            .zip(runners)
            .traverse { case ((rq, input), runner) =>
              runCompiledQuery(
                rq.query.value.mapK(WithSource.unwrap),
                input,
                runner,
              )
            }
            .void
      }

      private def runFile(documentUri: Uri): F[Unit] = {
        for {
          documentText <- TextDocumentProvider[F].get(documentUri)
          file <- SourceParser[SourceFile].parse(documentText).liftTo[F]
          compiledInputs <- compiler.compile(file)
          runners <- runner.get(file).leftMap(_.map(_._2)).leftMap(RunErrors(_)).liftTo[F]
          _ <- runCompiledFile(file, compiledInputs, runners)
        } yield ()
      }.recoverWith {
        case _: CompilationFailed | _: ParsingFailure =>
          CommandResultReporter[F].onCompilationFailed
        case e: RunErrors => e.report
      }

      private val commandMap: Map[String, List[String] => F[Unit]] = ListMap(
        Command.RUN_FILE -> {
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
