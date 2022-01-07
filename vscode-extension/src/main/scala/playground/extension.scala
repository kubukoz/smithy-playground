package playground

import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.effect.unsafe.implicits._
import cats.implicits._
import cats.parse.Parser.Expectation.InRange
import cats.~>
import com.disneystreaming.demo.smithy.CreateHeroOutput
import com.disneystreaming.demo.smithy.CreateSubscriptionOutput
import com.disneystreaming.demo.smithy.DemoService
import com.disneystreaming.demo.smithy.DemoServiceGen
import com.disneystreaming.demo.smithy.DemoServiceOperation
import com.disneystreaming.demo.smithy.Hero
import com.disneystreaming.demo.smithy.Subscription
import playground.Runner
import playground.smithyql.Query
import playground.smithyql.SmithyQLParser
import playground.smithyql.WithSource
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

import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExportTopLevel
import playground.smithyql.SourceRange

object extension {
  type EitherThrow[+A] = Either[Throwable, A]

  val eitherToIO: EitherThrow ~> IO =
    new (EitherThrow ~> IO) {
      override def apply[A](fa: EitherThrow[A]): IO[A] = fa.liftTo[IO]
    }

  val chan: OutputChannel = window.createOutputChannel("Smithy Playground")
  type Op[I, E, O, S, A] = DemoServiceOperation[I, E, O, S, A]

  val compiler: Compiler[Op, EitherThrow] = Compiler.instance(DemoServiceGen)
  val runnerDeff = Deferred.unsafe[IO, Runner[IO, Op]]
  val runner: Runner[IO, Op] = Runner.unlift(runnerDeff.get)

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

  @JSExportTopLevel("activate")
  def activate(
    context: ExtensionContext
  ): Unit = {
    mkRunner
      .evalMap(runnerDeff.complete(_))
      .allocated
      .unsafeRunAndForget()

    val _ = context
      .subscriptions
      .push(
        commands
          .registerTextEditorCommand(
            "smithyql.runQuery",
            (ted, _, _) =>
              run
                .perform[IO, Op](ted, compiler.mapK(eitherToIO), runner, chan)
                .unsafeRunAndForget(),
          ),
        languages.registerCodeLensProvider(
          "smithyql",
          mod.CodeLensProvider { (doc, _) =>
            validate(doc.getText())
              .map { case (parsed, _) =>
                new mod.CodeLens(
                  toVscodeRange(doc)(parsed.operationName.range),
                  mod.Command("smithyql.runQuery", "Run query"),
                )
              }
              .toList
              .toJSArray
          },
        ),
        languages.registerDocumentFormattingEditProvider(
          "smithyql",
          DocumentFormattingEditProvider { (doc, _, _) =>
            format.perform(doc)
          },
        ),
        vscodeutil.registerDiagnosticProvider("smithyql", getHighlights),
      )
  }

  private def validate(q: String): EitherThrow[(Query[WithSource], CompiledInput[Op])] =
    SmithyQLParser
      .parseFull(q)
      .leftWiden[Throwable]
      .mproduct(compiler.compile(_))

  private def getHighlights(
    doc: mod.TextDocument
  ): List[Diagnostic] =
    validate(doc.getText()) match {
      case Right(_) => Nil

      case Left(OperationNotFound(name, validOperations)) =>
        List(
          error(
            s"Operation not found. Available operations: ${validOperations.map(_.text).mkString_(", ")}",
            toVscodeRange(doc)(name.range),
          )
        )
      case Left(SmithyQLParser.ParsingFailure(e, _)) =>
        val pos = doc.positionAt(e.failedAtOffset.toDouble)
        val range = doc
          .getWordRangeAtPosition(pos)
          .getOrElse(new mod.Range(pos, doc.lineAt(doc.lineCount - 1).range.end))

        List(
          error(
            "Parsing failure: expected one of " + e
              .expected
              .map {
                case InRange(_, lower, upper) if lower == upper => lower.toString
                case InRange(_, lower, upper)                   => s"$lower-$upper"
                case msg                                        => msg.toString()
              }
              .mkString_(", "),
            range,
          )
        )

      case Left(e) =>
        val defaultRange =
          new mod.Range(doc.lineAt(0).range.start, doc.lineAt(doc.lineCount - 1).range.end)

        e match {
          case CompilationFailed(errors) =>
            errors.map { ee => // dźwig
              error(
                ee.message,
                ee.range.fold(defaultRange)(toVscodeRange(doc)),
              )
            }.toList

          case _ =>
            List(
              error(
                "Compilation failure: " + Option(e.getMessage()).getOrElse("null"),
                defaultRange,
              )
            )
        }

    }

  def toVscodeRange(doc: mod.TextDocument)(range: SourceRange): mod.Range = {
    val pos = doc.positionAt(range.start.index.toDouble)
    val end = doc.positionAt(range.end.index.toDouble)
    println(
      s"converting $range to vscode range: [${pos.line}, ${pos.character}] -> [${end.line}, ${end.character}]"
    )
    new mod.Range(pos, end)
  }

  private def error(msg: String, range: mod.Range) =
    new Diagnostic(range, msg, DiagnosticSeverity.Error)

}
