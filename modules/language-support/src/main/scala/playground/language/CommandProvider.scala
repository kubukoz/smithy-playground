package playground.language

import cats.MonadThrow
import cats.implicits._
import playground.smithyql.WithSource
import playground.Compiler
import playground.Runner
import scala.collection.immutable.ListMap
import playground.CompilationFailed
import playground.smithyql.parser.SourceParser
import playground.smithyql.Query

trait CommandProvider[F[_]] {
  def runCommand(name: String, args: List[String]): F[Unit]
}

object CommandProvider {

  def instance[F[_]: MonadThrow: TextDocumentProvider: CommandResultReporter](
    compiler: Compiler[F],
    runner: Runner.Resolver[F],
  ): CommandProvider[F] =
    new CommandProvider[F] {

      private def runQuery(documentUri: String): F[Unit] = TextDocumentProvider[F]
        .get(documentUri)
        .flatMap { documentText =>
          SourceParser[Query]
            .parse(documentText)
            .liftTo[F]
            .flatMap { parsed =>
              runner
                .get(parsed)
                .toEither
                .leftMap(Runner.Issue.squash(_))
                .leftMap {
                  case Left(protocols) => CommandResultReporter[F].onUnsupportedProtocol(protocols)
                  case Right(others)   => CommandResultReporter[F].onIssues(others)
                }
                .map { runner =>
                  compiler
                    .compile(parsed)
                    .flatMap { compiled =>
                      val parsedId = parsed.mapK(WithSource.unwrap)

                      CommandResultReporter[F]
                        .onQueryCompiled(parsedId, compiled)
                        .flatMap { requestId =>
                          runner
                            .run(compiled)
                            .flatMap {
                              CommandResultReporter[F].onQuerySuccess(parsedId, requestId, _)
                            }
                            .handleErrorWith {
                              CommandResultReporter[F].onQueryFailure(_, compiled, requestId)
                            }
                        }
                    }
                    .recoverWith { case _: CompilationFailed =>
                      CommandResultReporter[F].onCompilationFailed
                    }

                }
                .merge
            }
        }

      private val commandMap: Map[String, List[String] => F[Unit]] = ListMap(
        Command.RUN_QUERY -> {
          case documentUri :: Nil => runQuery(documentUri)
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
