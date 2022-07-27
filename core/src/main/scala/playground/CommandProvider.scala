package playground

import cats.MonadThrow
import cats.implicits._
import playground.smithyql.Formatter
import playground.smithyql.InputNode
import playground.smithyql.SmithyQLParser
import playground.smithyql.WithSource

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.immutable.ListMap

trait Feedback[F[_]] {
  def showErrorMessage(msg: String): F[Unit]
  def showOutputPanel: F[Unit]
  def logOutput(msg: String): F[Unit]
}

object Feedback {
  def apply[F[_]](implicit F: Feedback[F]): Feedback[F] = F
}

trait CommandProvider[F[_]] {
  def runCommand(name: String, args: List[String]): F[Unit]
}

object CommandProvider {

  def instance[F[_]: MonadThrow: TextDocumentProvider: Feedback](
    compiler: Compiler[F],
    runner: Runner.Optional[F],
  ): CommandProvider[F] =
    new CommandProvider[F] {
      // todo mutability
      private val requestCount = new AtomicInteger(0)

      private def runQuery(documentUri: String): F[Unit] = TextDocumentProvider[F]
        .get(documentUri)
        .flatMap { documentText =>
          SmithyQLParser
            .parseFull(documentText)
            .liftTo[F]
            .flatMap { parsed =>
              runner
                .get(parsed)
                .toEither
                .leftMap(Runner.Issue.squash(_))
                .leftMap {
                  case Left(protocols) =>
                    Feedback[F].showErrorMessage(
                      s"""The service uses an unsupported protocol.
                             |Supported protocols: ${protocols
                          .supported
                          .map(_.show)
                          .mkString_(", ")}
                             |Found protocols: ${protocols
                          .found
                          .map(_.show)
                          .mkString(", ")}""".stripMargin
                    )

                  case Right(others) =>
                    Feedback[F].showErrorMessage(
                      others.map(_.toString).mkString_("\n\n")
                    )
                }
                .map { runner =>
                  compiler
                    .compile(parsed)
                    .flatMap { compiled =>
                      val requestId = requestCount.addAndGet(1)

                      Feedback[F].showOutputPanel *>
                        Feedback[F].logOutput(
                          s"// Calling ${parsed.operationName.value.text} ($requestId)"
                        ) *>
                        runner
                          .run(compiled)
                          .onError { case e =>
                            val rendered =
                              compiled
                                .catchError(e)
                                .flatMap(err => compiled.writeError.map(_.toNode(err))) match {
                                case Some(e) => "\n" + writeOutput(e)
                                case None    => e.toString
                              }

                            Feedback[F].logOutput(s"// ERROR ($requestId) $rendered")
                          }
                          .flatMap { out =>
                            Feedback[F].logOutput(
                              s"// Succeeded ${parsed.operationName.value.text} ($requestId), response:\n"
                                + writeOutput(out)
                            )
                          }
                    }
                }
                .merge
            }
        }

      private def writeOutput(
        node: InputNode[cats.Id]
      ) = Formatter.writeAst(node.mapK(WithSource.liftId)).renderTrim(80)

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
