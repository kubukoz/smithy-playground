package playground

import cats.effect.kernel.Sync
import cats.implicits._
import playground.smithyql.SmithyQLParser
import typings.vscode.mod.OutputChannel
import typings.vscode.mod.TextEditor
import playground.smithyql.Formatter
import playground.smithyql.WithSource
import playground.smithyql.InputNode
import cats.effect.std
import java.util.concurrent.atomic.AtomicInteger
import cats.effect.kernel.Ref
import cats.effect.IO

object run {
  private val requestCount = new AtomicInteger(0)

  def perform[F[_]: Sync, Op[_, _, _, _, _]](
    ted: TextEditor,
    compiler: Compiler[F],
    runner: Runner[F],
    channel: OutputChannel,
  ): F[Unit] = SmithyQLParser
    .parseFull(ted.document.getText())
    .liftTo[F]
    .flatMap { parsed =>
      compiler
        .compile(parsed)
        .flatMap { compiled =>
          val requestId = requestCount.addAndGet(1)

          Sync[F].delay(channel.show(true)) *>
            Sync[F].delay(
              channel.appendLine(s"// Calling ${parsed.operationName.value.text} ($requestId)")
            ) *>
            runner
              .run(compiled)
              .onError { case e =>
                val rendered =
                  compiled
                    .catchError(e)
                    .flatMap(err => compiled.writeError.map(_.toNode(err))) match {
                    case Some(e) => writeOutput(e)
                    case None    => e.toString
                  }

                Sync[F].delay(channel.appendLine(s"// ERROR ($requestId)\n$rendered"))
              }
              .flatMap { out =>
                Sync[F].delay {
                  channel.appendLine(
                    s"// Succeeded ${parsed.operationName.value.text} ($requestId), response:\n"
                  )
                  channel.appendLine(writeOutput(out))
                }
              }
        }
    }

  private def writeOutput(
    node: InputNode[cats.Id]
  ) = Formatter.writeAst(node.mapK(WithSource.liftId)).renderTrim(80)

}
