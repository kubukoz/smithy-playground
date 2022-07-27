package playground

import cats.Applicative
import cats.MonadThrow
import cats.effect.std
import cats.implicits._
import playground.smithyql.Formatter
import playground.smithyql.InputNode
import playground.smithyql.SmithyQLParser
import playground.smithyql.WithSource

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.immutable.ListMap

trait CommandProvider[F[_]] {
  def listAvailableCommands: List[String]
  def runCommand(name: String, args: List[String]): F[Unit]
}

object CommandProvider {

  def instance[F[_]: MonadThrow: TextDocumentManager: std.Console](
    compiler: Compiler[F],
    runner: Runner.Optional[F],
  ): CommandProvider[F] =
    new CommandProvider[F] {
      // todo mutability
      private val requestCount = new AtomicInteger(0)

      private def runQuery(documentUri: String): F[Unit] = TextDocumentManager[F]
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
                    // todo: show client error
                    // IO(
                    //   window.showErrorMessage(
                    //     s"""The service uses an unsupported protocol.
                    //          |Supported protocols: ${protocols
                    //         .supported
                    //         .map(_.show)
                    //         .mkString_(", ")}
                    //          |Found protocols: ${protocols
                    //         .found
                    //         .map(_.show)
                    //         .mkString(", ")}""".stripMargin
                    //   )
                    // ).void
                    Applicative[F].unit

                  case Right(others) =>
                    // todo
                    // IO(
                    //   window.showErrorMessage(
                    //     others.map(_.toString).mkString_("\n\n")
                    //   )
                    // ).void
                    Applicative[F].unit
                }
                .map { runner =>
                  compiler
                    .compile(parsed)
                    .flatMap { compiled =>
                      val requestId = requestCount.addAndGet(1)

                      // todo
                      // Sync[F].delay(channel.show(true)) *>
                      //   Sync[F].delay(
                      //     channel.appendLine(
                      //       s"// Calling ${parsed.operationName.value.text} ($requestId)"
                      //     )
                      //   ) *>
                      runner
                        .run(compiled)
                        // todo
                        // .onError { case e =>
                        //   val rendered =
                        //     compiled
                        //       .catchError(e)
                        //       .flatMap(err => compiled.writeError.map(_.toNode(err))) match {
                        //       case Some(e) => "\n" + writeOutput(e)
                        //       case None    => e.toString
                        //     }

                        //   Sync[F].delay(channel.appendLine(s"// ERROR ($requestId) $rendered"))
                        // }
                        .flatMap { out =>
                          std.Console[F].println {
                            s"// Succeeded ${parsed.operationName.value.text} ($requestId), response:\n"
                              + writeOutput(out)
                          }
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
        "smithyql.runQuery" -> {
          case documentUri :: Nil => runQuery(documentUri)
          case s => new Throwable("Unsupported arguments: " + s).raiseError[F, Unit]
        }
      )

      val listAvailableCommands: List[String] = commandMap.keys.toList

      def runCommand(
        name: String,
        args: List[String],
      ): F[Unit] = commandMap
        .get(name)
        .liftTo[F](new Throwable("Unsupported command:  " + name))
        .flatMap(_.apply(args))

    }

}
