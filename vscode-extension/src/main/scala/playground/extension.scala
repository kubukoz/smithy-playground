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
import playground.Runner.Issue.Other
import cats.effect.Resource
import cats.effect.implicits._
import util.chaining._
import typings.vscode.anon.Dispose
import smithy4s.dynamic.DynamicSchemaIndex

object extension {
  private val chan: OutputChannel = window.createOutputChannel("Smithy Playground")
  private var shutdownHook: IO[Unit] = IO.unit

  private def timedResource[A](tag: String)(res: Resource[IO, A]): Resource[IO, A] = res
    .timed
    .evalMap { case (fd, value) => IO.println(s"$tag took ${fd.toMillis}ms").as(value) }

  @JSExportTopLevel("activate")
  def activate(
    context: ExtensionContext
  ): Unit = client
    .make[IO](useNetwork = false)
    .flatMap(activateR(context, _))
    .pipe(timedResource("activateR"))
    .allocated
    .onError { case e => std.Console[IO].printStackTrace(e) }
    .flatMap { case (_, shutdown) => IO { shutdownHook = shutdown } }
    .unsafeRunAndForget()

  @JSExportTopLevel("deactivate")
  def deactivate(): Unit = shutdownHook.unsafeRunAndForget()

  private def activateR(
    context: ExtensionContext,
    client: Client[IO],
  ): Resource[IO, Unit] =
    build
      .buildFile[IO](chan)
      .toResource
      .pipe(timedResource("buildFile"))
      .map(build.getServices(_, chan))
      .flatMap { dsi =>
        // todo: up for removal
        val service = dsi.allServices.head

        Runner
          .make(
            service.service,
            client,
            vscodeutil.getConfigF[IO, String]("smithyql.http.baseUrl").flatMap {
              Uri
                .fromString(_)
                .liftTo[IO]
            },
          )
          .flatMap { implicit runner =>
            Resource.make {
              IO {
                debug.timed("activateInternal") {
                  activateInternal(
                    context,
                    dsi,
                    service.service,
                  )
                }
              }
            }(subs => IO(subs.foreach(_.dispose())))
          }
      }
      .void

  private def activateInternal[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
    context: ExtensionContext,
    dsi: DynamicSchemaIndex,
    service: Service[Alg, Op],
  )(
    implicit runner: Runner.Optional[IO]
  ): List[mod.Disposable] = {

    implicit val compiler: Compiler[EitherThrow] =
      debug.timed("compiler setup") {
        Compiler.fromSchemaIndex(dsi)
      }

    import vscodeutil.disposableToDispose

    val completionProvider =
      debug.timed("completionProvider setup")(completions.complete(service.service))

    chan.appendLine("Smithy Playground activated! Info to follow:")
    chan.appendLine(s"""Service: ${service.service.id.show}
    |Operations: ${service.service.endpoints.map(_.name).mkString("\n")}
    |""".stripMargin)

    val subs = List(
      commands
        .registerTextEditorCommand(
          "smithyql.runQuery",
          (ted, _, _) =>
            {
              runner.get match {
                case Left(e) =>
                  e match {
                    case Runner.Issue.InvalidProtocols(ps) =>
                      IO(
                        window.showErrorMessage(
                          s"The service uses an unsupported protocol. Available protocols: ${ps.map(_.show).mkString_(", ")}"
                        )
                      ).void

                    case Other(e) =>
                      IO(
                        window.showErrorMessage(
                          e.toString()
                        )
                      )
                  }

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
                .full[EitherThrow](doc.getText())
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
      vscodeutil.registerDiagnosticProvider("smithyql", highlight.getHighlights[IO]),
    )

    val _ = context
      .subscriptions
      .push(subs.map(identity(_): Dispose): _*)

    subs
  }

}
