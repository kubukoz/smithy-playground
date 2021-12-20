package playground

import cats.Id
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.kernel.Resource
import cats.effect.std
import cats.implicits._
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.implicits._
import playground._
import smithy4s.Endpoint
import smithy4s.Service
import smithy4s.http4s.SimpleRestJsonBuilder

import java.nio.file.Files
import java.nio.file.Paths
import java.time.Instant
import com.disneystreaming.deployship.smithy.DeployshipServiceGen
import smithy4s.http4s.SimpleProtocolBuilder
import smithy4s.http.CodecAPI
import smithy4s.Constraints
import com.disneystreaming.demo.smithy.DemoServiceGen
import com.disneystreaming.demo.smithy.DemoService
import org.http4s.HttpApp
import org.http4s.implicits._
import com.disneystreaming.demo.smithy.CreateHeroOutput
import com.disneystreaming.demo.smithy.Hero
import com.disneystreaming.demo.smithy.CreateSubscriptionOutput
import com.disneystreaming.demo.smithy.Subscription

trait Compiler[Op[_, _, _, _, _], F[_]] { self =>
  def compile(q: Query): F[Op[_, _, _, _, _]]
}

private class CompilerImpl[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
  service: Service[Alg, Op]
) extends Compiler[Op, Id] {

  private val schem = new QuerySchematic

  // for quick lookup and decoding from AST
  private val endpoints: Map[String, AST => Op[_, _, _, _, _]] = {
    def go[I](
      endpoint: Endpoint[Op, I, _, _, _, _]
    ) = endpoint.input.compile(schem).andThen(endpoint.wrap)

    service
      .endpoints
      .groupByNel(_.name)
      .map(_.map(_.head).map(go(_)))
  }

  def compile(q: Query): Op[_, _, _, _, _] =
    endpoints.getOrElse(
      q.operationName,
      throw new Exception(
        s"Operation not found: ${q.operationName}. Available operations: ${endpoints.keys.mkString(", ")}"
      ),
    )(q.input)

}

trait Runner[F[_]] {
  def run(q: Query): F[Any]
}

object Runner {

  def make[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
    service: Service[Alg, Op],
    impl: Option[HttpApp[IO]] = None,
  ): Resource[IO, Runner[IO]] = EmberClientBuilder
    .default[IO]
    .build
    .evalMap { c =>
      val compiler: Compiler[Op, Id] = new CompilerImpl(service)

      val b = SimpleRestJsonBuilder(service)

      impl
        .fold(
          b.client(c, uri"http://localhost:8082")
        )(b.client(_, uri"http://localhost:8082"))
        .map { client =>
          val exec = service.asTransformation(client)

          q => IO.defer(exec(compiler.compile(q)))
        }
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
      .eval(
        SimpleRestJsonBuilder
          .routes(new DemoService[IO] {

            override def createHero(
              hero: Hero
            ): IO[CreateHeroOutput] = IO(CreateHeroOutput(hero))

            override def createSubscription(
              subscription: Subscription
            ): IO[CreateSubscriptionOutput] = IO(CreateSubscriptionOutput(subscription))

          })
          .make
      )
      .flatMap { routes =>
        fs2
          .Stream
          .resource(
            Runner.make(
              DemoServiceGen,
              routes.orNotFound.some,
            )
          )
      }
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
