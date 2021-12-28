package playground

import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.effect.unsafe.implicits._
import cats.implicits._
import cats.parse.Parser.Expectation.InRange
import com.disneystreaming.demo.smithy.CreateHeroOutput
import com.disneystreaming.demo.smithy.CreateSubscriptionOutput
import com.disneystreaming.demo.smithy.DemoService
import com.disneystreaming.demo.smithy.DemoServiceGen
import com.disneystreaming.demo.smithy.Hero
import com.disneystreaming.demo.smithy.Subscription
import playground.Runner
import smithy4s.http4s.SimpleRestJsonBuilder
import typings.vscode.anon.Dispose
import typings.vscode.mod
import typings.vscode.mod.Diagnostic
import typings.vscode.mod.DiagnosticSeverity
import typings.vscode.mod.Disposable
import typings.vscode.mod.DocumentFormattingEditProvider
import typings.vscode.mod.ExtensionContext
import typings.vscode.mod.OutputChannel
import typings.vscode.mod.commands
import typings.vscode.mod.languages
import typings.vscode.mod.window
import typings.vscode.mod.workspace

import scala.scalajs.js._
import scala.scalajs.js.annotation.JSExportTopLevel

object extension {
  val chan: OutputChannel = window.createOutputChannel("Smithy Playground")
  val runnerDeff = Deferred.unsafe[IO, Runner[IO]]
  val runner: Runner[IO] = Runner.unlift(runnerDeff.get)

  val mkRunner = SimpleRestJsonBuilder
    .routes(new DemoService[IO] {

      override def createHero(
        hero: Hero
      ): IO[CreateHeroOutput] = IO(CreateHeroOutput(hero))

      override def createSubscription(
        subscription: Subscription
      ): IO[CreateSubscriptionOutput] = IO(CreateSubscriptionOutput(subscription))

    })
    .resource
    .flatMap { routes =>
      Runner
        .make(DemoServiceGen, Some(routes.orNotFound))
    }

  implicit def disposableToDispose(d: Disposable): Dispose = Dispose(() => d.dispose())

  val errors = languages.createDiagnosticCollection()

  @JSExportTopLevel("activate")
  def activate(
    context: ExtensionContext
  ): Unit = {
    mkRunner
      .evalMap(runnerDeff.complete(_))
      .allocated
      .unsafeRunAndForget()

    window.activeTextEditor.foreach { ted =>
      performHighlight(ted.document)
    }

    val _ = context
      .subscriptions
      .push(
        commands
          .registerTextEditorCommand(
            "smithyql.runQuery",
            (ted, _, _) =>
              run
                .perform[IO](ted, runner, chan)
                .unsafeRunAndForget(),
          ),
        languages.registerCodeLensProvider(
          "smithyql",
          mod.CodeLensProvider((doc, _) =>
            Array(
              new mod.CodeLens(
                doc.lineAt(0).range,
                mod.Command("smithyql.runQuery", "Run Smithy Query"),
              )
            )
          ),
        ),
        languages.registerDocumentFormattingEditProvider(
          "smithyql",
          DocumentFormattingEditProvider { (doc, _, _) =>
            format.perform(doc)
          },
        ),
        window.onDidChangeActiveTextEditor(
          _.map(_.document).foreach(performHighlight),
          null,
          null,
        ),
        workspace
          .onDidSaveTextDocument
          .apply(
            performHighlight,
            (),
            (),
          ),
        workspace.onDidCloseTextDocument(
          doc => errors.delete(doc.uri),
          null,
          null,
        ),
      )

    val _ = window.showInformationMessage(
      """Smithy Playground is open! Start by opening the Command Pallette and running the "Run SmithyQL Query" command.""".stripMargin
    )

  }

  private def performHighlight(
    doc: mod.TextDocument
  ) =
    if (doc.languageId == "smithyql")
      errors.set(doc.uri, Array(highlights(doc): _*))

  private def highlights(doc: mod.TextDocument): List[Diagnostic] = {
    val parsed = SmithyQLParser.parser.parseAll(doc.getText())

    parsed match {
      case Right(_) => Nil

      case Left(e) =>
        val pos = doc.positionAt(e.failedAtOffset.toDouble)
        val range = new mod.Range(pos, doc.lineAt(doc.lineCount - 1).range.end)

        List(
          new Diagnostic(
            range,
            "Parsing failure: expected one of " + e
              .expected
              .map {
                case InRange(_, lower, upper) if lower == upper => lower.toString
                case InRange(_, lower, upper)                   => s"$lower-$upper"
                case msg                                        => msg.toString()
              }
              .mkString_(", "),
            DiagnosticSeverity.Error,
          )
        )
    }
  }

}
