package playground

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.implicits._
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.implicits._
import playground._
import smithy4s.Endpoint
import smithy4s.Service
import smithy4s.http4s.SimpleRestJsonBuilder

class Compiler[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
  service: Service[Alg, Op]
) {

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

  def compile(q: Query): Op[_, _, _, _, _] = endpoints(q.operationName)(q.input)

}

trait Runner[F[_]] {
  def run(q: Query): F[Any]
}

object Runner {

  def make[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
    service: Service[Alg, Op]
  ): Resource[IO, Runner[IO]] = EmberClientBuilder
    .default[IO]
    .build
    .evalMap { c =>
      val compiler = new Compiler(service)

      SimpleRestJsonBuilder(service).client(c, uri"http://localhost:8082").map { client =>
        val exec = service.asTransformation(client)

        q => exec(compiler.compile(q))
      }
    }

}
