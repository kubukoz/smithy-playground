package playground

import cats.effect.IO
import cats.effect.std
import cats.effect.unsafe.implicits._
import cats.implicits._
import org.http4s.Uri
import org.http4s.client.Client
import playground.Runner
import smithy4s.Service
import typings.vscode.mod
import typings.vscode.mod.DocumentFormattingEditProvider
import typings.vscode.mod.ExtensionContext
import typings.vscode.mod.OutputChannel
import typings.vscode.mod.commands
import typings.vscode.mod.languages
import typings.vscode.mod.window

import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExportTopLevel

import types._

object extension {
  private val chan: OutputChannel = window.createOutputChannel("Smithy Playground")
  private var shutdownHook: IO[Unit] = IO.unit

  @JSExportTopLevel("activate")
  def activate(
    context: ExtensionContext
  ): Unit = client
    .make[IO](useNetwork = false)
    .evalMap(activateIO(context, _))
    .allocated
    .onError { case e => std.Console[IO].printStackTrace(e) }
    .flatMap { case (_, shutdown) => IO { shutdownHook = shutdown } }
    .unsafeRunAndForget()

  @JSExportTopLevel("deactivate")
  def deactivate(): Unit = shutdownHook.unsafeRunAndForget()

  // No resources allowed here
  def activateIO(
    context: ExtensionContext,
    client: Client[IO],
  ): IO[Unit] = build
    .buildFile[IO]
    .map(build.getService(_))
    .flatMap { service =>
      Uri
        .fromString(vscodeutil.unsafeGetConfig[String]("smithyql.http.baseUrl"))
        .liftTo[IO]
        .flatMap { baseUri =>
          implicit val runnerOpt
            : Runner.Optional[IO, service.Op] = Runner.make(service.service, client, baseUri)

          IO {
            activateInternal(
              context,
              service.service,
            )
          }
        }
    }

  def activateInternal[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
    context: ExtensionContext,
    service: Service[Alg, Op],
  )(
    implicit runner: Runner.Optional[IO, Op]
  ): Unit = {
    implicit val compiler: Compiler[Op, EitherThrow] = Compiler.instance(
      service.service
    )

    import vscodeutil.disposableToDispose

    val completionProvider = completions.complete(service.service)

    val _ = context
      .subscriptions
      .push(
        commands
          .registerTextEditorCommand(
            "smithyql.runQuery",
            (ted, _, _) =>
              {
                runner.get match {
                  case Left(e) =>
                    IO(
                      window.showErrorMessage(
                        s"Unsupported protocol for service ${e.service.id.show}: ${e.protocolTag.id.show}"
                      )
                    ).void

                  case Right(runner) =>
                    run.perform[IO, Op](ted, compiler.mapK(eitherToIO), runner, chan)
                }
              }.unsafeRunAndForget(),
          ),
        languages.registerCompletionItemProvider(
          "smithyql",
          mod
            .CompletionItemProvider { (doc, pos, _, _) =>
              completionProvider(doc, pos).toJSArray
            },
          // todo this might not be working properly
          "\t",
        ),
        languages.registerCodeLensProvider(
          "smithyql",
          mod.CodeLensProvider { (doc, _) =>
            {
              if (runner.get.isRight)
                validate
                  .full[Op, EitherThrow](doc.getText())
                  .map { case (parsed, _) =>
                    new mod.CodeLens(
                      adapters.toVscodeRange(doc, parsed.operationName.range),
                      mod.Command("smithyql.runQuery", "Run query"),
                    )
                  }
                  .toList
              else
                Nil
            }.toJSArray
          },
        ),
        languages.registerDocumentFormattingEditProvider(
          "smithyql",
          DocumentFormattingEditProvider { (doc, _, _) =>
            format.perform(doc).toJSArray
          },
        ),
        vscodeutil.registerDiagnosticProvider("smithyql", highlight.getHighlights[Op, IO]),
      )
  }

}
