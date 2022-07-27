package playground

import cats.Show
import cats.effect.IO
import cats.effect.Resource
import cats.effect.implicits._
import cats.effect.std
import cats.effect.unsafe.implicits._
import cats.implicits._
import fs2.io.file.Path
import org.http4s.Uri
import org.http4s.client.Client
import playground.Runner
import playground.smithyql.SmithyQLParser
import smithy4s.aws.AwsEnvironment
import smithy4s.aws.http4s.AwsHttp4sBackend
import smithy4s.aws.kernel.AwsRegion
import typings.vscode.anon.Dispose
import typings.vscode.mod
import typings.vscode.mod.ExtensionContext
import typings.vscode.mod.OutputChannel
import typings.vscode.mod.commands
import typings.vscode.mod.languages
import typings.vscode.mod.window
import typings.vscodeLanguageclient.clientMod.LanguageClientOptions
import typings.vscodeLanguageserverProtocol.protocolMod

import java.nio.charset.Charset
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExportTopLevel

import types._
import util.chaining._
import debug.UnsafeLog

object extension {
  private val chan: OutputChannel = window.createOutputChannel("Smithy Playground", "smithyql")

  implicit val ioConsole: std.Console[IO] =
    new std.Console[IO] {

      def readLineWithCharset(
        charset: Charset
      ): IO[String] = IO.consoleForIO.readLineWithCharset(charset)

      def print[A](a: A)(implicit S: Show[A]): IO[Unit] = IO(chan.append("// " + a.show))

      def println[A](a: A)(implicit S: Show[A]): IO[Unit] = IO(chan.appendLine("// " + a.show))

      def error[A](a: A)(implicit S: Show[A]): IO[Unit] = print("ERROR: " + a)

      def errorln[A](a: A)(implicit S: Show[A]): IO[Unit] = println("ERROR: " + a)
    }

  private implicit val unsafeLog: UnsafeLog = s => chan.appendLine("// " + s)

  private var shutdownHook: IO[Unit] = IO.unit

  private def timedResource[A](tag: String)(res: Resource[IO, A]): Resource[IO, A] = res
    .timed
    .evalMap { case (fd, value) => IO.println(s"$tag took ${fd.toMillis}ms").as(value) }

  @JSExportTopLevel("activate")
  def activate(
    context: ExtensionContext
  ): Unit = {
    val lspClient =
      new LanguageClient(
        "smithyPlayground",
        "Smithy Playground Client",
        ServerOptions(
          "/Users/kubukoz/projects/smithy-playground/lsp/target/jvm-2.13/universal/stage/bin/lsp"
        ),
        LanguageClientOptions().setDocumentSelectorVarargs(
          mod
            .DocumentFilter()
            .setLanguage("smithyql")
            .asInstanceOf[protocolMod.DocumentFilter]
        ),
      )

    context.subscriptions.push(lspClient.start())
    chan.appendLine("Connected client")
    activateClient(context)
  }

  @JSExportTopLevel("deactivate")
  def deactivate(): Unit = shutdownHook.unsafeRunAndForget()

  def activateClient(
    context: ExtensionContext
  ): Unit = client
    .make[IO]
    .flatMap(activateR(context, _))
    .pipe(timedResource("activateR"))
    .allocated
    .onError { case e => std.Console[IO].printStackTrace(e) }
    .flatMap { case (_, shutdown) => IO { shutdownHook = shutdown } }
    .unsafeRunAndForget()

  private def activateR(
    context: ExtensionContext,
    client: Client[IO],
  ): Resource[IO, Unit] =
    build
      .buildFile[IO]
      .toResource
      .pipe(timedResource("buildFile"))
      .evalMap { case (buildConfig, buildConfigUri) =>
        build.getServices(
          Path(buildConfigUri.fsPath)
            .parent
            .getOrElse(sys.error("Couldn't find parent of " + buildConfigUri))
            .toString,
          buildConfig,
        )
      }
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
            val compiler: Compiler[IorThrow] =
              debug.timed("compiler setup") {
                Compiler.fromSchemaIndex(dsi)
              }

            Resource
              .make {
                IO {
                  debug.timed("activateInternal") {
                    activateInternal(
                      context,
                      compiler,
                      runner,
                    )
                  }
                }
              }(subs => IO(subs.foreach(_.dispose())))
              .evalTap { _ =>
                std.Console[IO].println("Smithy Playground activated!")
              }
          }
      }
      .void

  private def activateInternal[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
    context: ExtensionContext,
    compiler: Compiler[IorThrow],
    runner: Runner.Optional[IO],
  ): List[mod.Disposable] = {

    import vscodeutil.disposableToDispose

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
                      ).void

                    case Right(others) =>
                      IO(
                        window.showErrorMessage(
                          others.map(_.toString).mkString_("\n\n")
                        )
                      ).void
                  }
                  .map { runner =>
                    run.perform[IO, Op](ted, compiler.mapK(iorToIO), runner, chan)
                  }
                  .merge
              }
              .unsafeRunAndForget(),
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
