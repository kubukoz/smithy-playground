package playground

import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.effect.unsafe.implicits._
import com.disneystreaming.demo.smithy.CreateHeroOutput
import com.disneystreaming.demo.smithy.CreateSubscriptionOutput
import com.disneystreaming.demo.smithy.DemoService
import com.disneystreaming.demo.smithy.DemoServiceGen
import com.disneystreaming.demo.smithy.Hero
import com.disneystreaming.demo.smithy.Subscription
import playground.Runner
import smithy4s.http4s.SimpleRestJsonBuilder
import typings.vscode.anon.Dispose
import typings.vscode.mod.Disposable
import typings.vscode.mod.DocumentFormattingEditProvider
import typings.vscode.mod.ExtensionContext
import typings.vscode.mod.OutputChannel
import typings.vscode.mod.commands
import typings.vscode.mod.languages
import typings.vscode.mod.window

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
                .perform[IO](ted, runner, chan)
                .unsafeRunAndForget(),
          ),
        languages.registerDocumentFormattingEditProvider(
          "smithyql",
          DocumentFormattingEditProvider { (doc, _, _) =>
            format.perform(doc)
          },
        ),
      )

    val _ = window.showInformationMessage(
      """Smithy Playground is open! Start by opening the Command Pallette and running the "Run SmithyQL Query" command.""".stripMargin
    )

  }

}
