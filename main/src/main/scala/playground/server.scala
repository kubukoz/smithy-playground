import cats.effect.IO

import cats.effect.IOApp
import cats.effect.std
import cats.implicits._
import com.comcast.ip4s._
import com.disneystreaming.demo.smithy.CreateHeroOutput
import com.disneystreaming.demo.smithy.CreateSubscriptionOutput
import com.disneystreaming.demo.smithy.DemoService
import com.disneystreaming.demo.smithy.DemoServiceGen
import com.disneystreaming.demo.smithy.Hero
import com.disneystreaming.demo.smithy.PlaygroundService
import com.disneystreaming.demo.smithy.RunQueryOutput
import com.disneystreaming.demo.smithy.Subscription
import org.http4s.ember.server.EmberServerBuilder
import playground.Runner
import playground.SmithyQLParser
import smithy4s.http4s.SimpleRestJsonBuilder

import java.nio.file.Files
import java.nio.file.Paths
import java.time.Instant

object Server extends IOApp.Simple {

  def run: IO[Unit] =
    mkRunner.flatMap { runner =>
      SimpleRestJsonBuilder
        .routes(new PlaygroundService[IO] {
          def runQuery(input: String): IO[RunQueryOutput] = IO(SmithyQLParser.parse(input))
            .flatMap(runner.run)
            .attempt
            .map(_.toString)
            .map(RunQueryOutput(_))
        })
        .resource
        .flatMap { routes =>
          EmberServerBuilder
            .default[IO]
            .withHttpApp(routes.orNotFound)
            .withHost(host"localhost")
            .withPort(port"4000")
            .build
        }
    }.useForever

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
      Runner.make(
        DemoServiceGen,
        routes.orNotFound.some,
      )
    }

}

object Main extends IOApp.Simple {

  val clear = "\u001b[2J\u001b[H"

  def handled(
    e: Throwable
  ): IO[Unit] = std.Console[IO].errorln(Option(e.getMessage()).getOrElse(e.toString()))

  def run: IO[Unit] =
    fs2
      .Stream
      .resource(Server.mkRunner)
      .flatMap { runner =>
        fs2
          .Stream
          .repeatEval(IO(Files.readString(Paths.get("../example.smithyql"))))
          .filterNot(_.trim.isEmpty())
          .changes
          .evalMap { s =>
            IO(SmithyQLParser.parse(s))
              .onError(IO.println(clear) *> handled(_))
              .flatMap { q =>
                IO.println(clear) *>
                  IO.println("Parsed! " + Instant.now()) *>
                  IO(pprint.pprintln(q)) *>
                  runner.run(q).flatMap(a => IO(pprint.pprintln(a))).handleErrorWith(handled)
              }
              .attempt
          }
      }
      .compile
      .drain

}
