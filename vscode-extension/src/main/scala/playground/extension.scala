package playground

import cats.effect.IO
import cats.effect.Resource
import cats.effect.implicits._
import cats.effect.std
import cats.effect.unsafe.implicits._
import cats.implicits._
import org.http4s.Uri
import org.http4s.client.Client
import playground.Runner
import playground.smithyql.SmithyQLParser
import smithy4s.aws.AwsEnvironment
import smithy4s.aws.http4s.AwsHttp4sBackend
import smithy4s.aws.kernel.AwsRegion
import typings.vscode.anon.Dispose
import typings.vscode.mod
import typings.vscode.mod.DocumentFormattingEditProvider
import typings.vscode.mod.ExtensionContext
import typings.vscode.mod.OutputChannel
import typings.vscode.mod.commands
import typings.vscode.mod.languages
import typings.vscode.mod.window
import typings.vscodeLanguageclient.clientMod.LanguageClientOptions
import typings.vscodeLanguageclient.mod.LanguageClient
import typings.vscodeLanguageclient.mod._ServerOptions
import typings.vscodeLanguageserverProtocol.protocolMod

import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExportTopLevel

import types._
import util.chaining._

object extension {
  private val chan: OutputChannel = window.createOutputChannel("Smithy Playground")
  private var shutdownHook: IO[Unit] = IO.unit

  private def timedResource[A](tag: String)(res: Resource[IO, A]): Resource[IO, A] = res
    .timed
    .evalMap { case (fd, value) => IO.println(s"$tag took ${fd.toMillis}ms").as(value) }

  val lspClient =
    new LanguageClient(
      "smithyPlayground",
      "Smithy Playground Client",
      _ServerOptions.Executable(
        "/Users/kubukoz/projects/smithy-playground/lsp/target/jvm-2.13/universal/stage/bin/lsp"
      ),
      LanguageClientOptions().setDocumentSelectorVarargs(
        mod
          .DocumentFilter()
          .setLanguage("smithyql")
          .asInstanceOf[protocolMod.DocumentFilter]
      ),
    )

  var disp: mod.Disposable = null

  @JSExportTopLevel("activate")
  def activateWithClient(
    context: ExtensionContext
  ): Unit = {

    disp = lspClient.start()
    chan.appendLine("Connected client")
  }

  @JSExportTopLevel("activate_old")
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
  def deactivate(): Unit = disp.dispose()

  // @JSExportTopLevel("deactivate")
  // def deactivate(): Unit = shutdownHook.unsafeRunAndForget()

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
        AwsEnvironment
          .default(AwsHttp4sBackend(client), AwsRegion.US_EAST_1)
          .memoize
          .map { awsEnv =>
            Runner
              .forSchemaIndex(
                dsi,
                client,
                vscodeutil.getConfigF[IO, String]("smithyql.http.baseUrl").flatMap {
                  Uri
                    .fromString(_)
                    .liftTo[IO]
                },
                awsEnv,
              )
          }
          .flatMap { runner =>
            val compiler: Compiler[EitherThrow] =
              debug.timed("compiler setup") {
                Compiler.fromSchemaIndex(dsi)
              }

            Resource.make {
              IO {
                debug.timed("activateInternal") {
                  activateInternal(
                    context,
                    compiler,
                    CompletionProvider.forSchemaIndex(dsi),
                    runner,
                  )
                }
              }
            }(subs => IO(subs.foreach(_.dispose())))
          }
      }
      .void

  private def activateInternal[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
    context: ExtensionContext,
    compiler: Compiler[EitherThrow],
    completionProvider: CompletionProvider,
    runner: Runner.Optional[IO],
  ): List[mod.Disposable] = {

    import vscodeutil.disposableToDispose

    val vscodeCompletionProvider =
      debug.timed("vscodeCompletionProvider setup")(
        completions.complete(completionProvider)
      )

    chan.appendLine("Smithy Playground activated!")

    val subs = List(
      commands
        .registerTextEditorCommand(
          "smithyql.runQuery",
          (ted, _, _) =>
            SmithyQLParser
              .parseFull(ted.document.getText())
              .liftTo[IO]
              .flatMap { parsed =>
                runner
                  .get(parsed)
                  .toEither
                  .leftMap(Runner.Issue.squash(_))
                  .leftMap {
                    case Left(protocols) =>
                      IO(
                        window.showErrorMessage(
                          s"The service uses an unsupported protocol. Available protocols: ${protocols.map(_.show).mkString_(", ")}"
                        )
                      ).void

                    case Right(others) =>
                      IO(
                        window.showErrorMessage(
                          others.map(_.toString).mkString_("\n\n")
                        )
                      ).void
                  }
                  .map { runner =>
                    run.perform[IO, Op](ted, compiler.mapK(eitherToIO), runner, chan)
                  }
                  .merge
              }
              .unsafeRunAndForget(),
        ),
      languages.registerCompletionItemProvider(
        "smithyql",
        mod
          .CompletionItemProvider { (doc, pos, _, _) =>
            vscodeCompletionProvider(doc, pos).toJSArray
          },
        // todo this might not be working properly
        "\t",
      ),
      languages.registerCodeLensProvider(
        "smithyql",
        mod.CodeLensProvider { (doc, _) =>
          {
            SmithyQLParser.parseFull(doc.getText()) match {
              case Right(parsed) if runner.get(parsed).toEither.isRight =>
                compiler
                  .compile(parsed)
                  .as {
                    new mod.CodeLens(
                      adapters.toVscodeRange(doc, parsed.operationName.range),
                      mod.Command("smithyql.runQuery", "Run query"),
                    )
                  }
                  .toList
              case _ => Nil
            }
          }.toJSArray
        },
      ),
      languages.registerDocumentFormattingEditProvider(
        "smithyql",
        DocumentFormattingEditProvider { (doc, _, _) =>
          format.perform(doc).toJSArray
        },
      ),
      vscodeutil.registerDiagnosticProvider(
        "smithyql",
        highlight.getHighlights(_, compiler, runner),
      ),
    )

    val _ = context
      .subscriptions
      .push(subs.map(identity(_): Dispose): _*)

    subs
  }

}
