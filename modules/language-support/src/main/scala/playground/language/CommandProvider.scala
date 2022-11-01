package playground.language

import cats.MonadThrow
import cats.implicits._
import playground.smithyql.WithSource
import playground.OperationRunner
import scala.collection.immutable.ListMap
import playground.CompilationFailed
import playground.smithyql.parser.SourceParser
import playground.smithyql.Query
import playground.FileCompiler
import playground.smithyql.SourceFile
import playground.CompiledInput
import playground.smithyql.parser.ParsingFailure

trait CommandProvider[F[_]] {
  def runCommand(name: String, args: List[String]): F[Unit]
}

object CommandProvider {

  def instance[F[_]: MonadThrow: TextDocumentProvider: CommandResultReporter](
    compiler: FileCompiler[F],
    runner: OperationRunner.Resolver[F],
  ): CommandProvider[F] =
    new CommandProvider[F] {

      private def runOneQuery(q: Query[WithSource], input: CompiledInput): F[Unit] =
        runner
          .get(q)
          .toEither
          .leftMap(OperationRunner.Issue.squash(_))
          .leftMap {
            case Left(protocols) => CommandResultReporter[F].onUnsupportedProtocol(protocols)
            case Right(others)   => CommandResultReporter[F].onIssues(others)
          }
          .map { runner =>
            val idQuery = q.mapK(WithSource.unwrap)

            CommandResultReporter[F]
              .onQueryCompiled(idQuery, input)
              .flatMap { requestId =>
                runner
                  .run(input)
                  .flatMap {
                    CommandResultReporter[F].onQuerySuccess(idQuery, requestId, _)
                  }
                  .handleErrorWith {
                    CommandResultReporter[F].onQueryFailure(_, input, requestId)
                  }
              }
          }
          .merge

      private def runQuery(documentUri: Uri): F[Unit] = TextDocumentProvider[F]
        .get(documentUri)
        .flatMap { documentText =>
          SourceParser[SourceFile]
            .parse(documentText)
            .liftTo[F]
            .flatMap { file =>
              compiler
                .compile(file)
                .flatMap { compiledInputs =>
                  file.statements.zip(compiledInputs).traverse_ { case (stat, input) =>
                    stat.fold(
                      runQuery = rq => runOneQuery(rq.query.value, input)
                    )
                  }
                }
            }
            .recoverWith { case _: CompilationFailed | _: ParsingFailure =>
              CommandResultReporter[F].onCompilationFailed
            }
        }

      private val commandMap: Map[String, List[String] => F[Unit]] = ListMap(
        Command.RUN_QUERY -> {
          case documentUri :: Nil => runQuery(Uri.fromUriString(documentUri))
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
