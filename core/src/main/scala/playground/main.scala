package playground

import cats.Id
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.implicits._
import org.http4s.HttpApp
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.implicits._
import playground._
import smithy4s.Endpoint
import smithy4s.Service
import smithy4s.http4s.SimpleRestJsonBuilder

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
    ) = endpoint.input.compile(schem).andThen(endpoint.wrap(_))

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

  def make[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _], F[_]](
    service: Service[Alg, Op],
    impl: Option[HttpApp[IO]] = None,
  ): Resource[IO, Runner[IO]] = EmberClientBuilder
    .default[IO]
    .build
    .flatMap { c =>
      val compiler: Compiler[Op, Id] = new CompilerImpl(service)

      val b = SimpleRestJsonBuilder(service)

      impl
        .fold(
          b.clientResource(c, uri"http://localhost:8082")
        )(b.clientResource(_, uri"http://localhost:8082"))
        .map { client =>
          val exec = service.asTransformation(client)

          q => IO.defer(exec(compiler.compile(q)))
        }
    }

}
