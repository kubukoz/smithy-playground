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

class Runner[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _], F[_]](
  service: Service[Alg, Op],
  client: Alg[smithy4s.GenLift[F]#λ],
) {

  private val exec = service.asTransformation(client)
  private val compiler = new Compiler(service)

  def run(q: Query) = exec(compiler.compile(q))
}

object Runner {

  def make[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
    service: Service[Alg, Op]
  ): Resource[IO, Runner[Alg, Op, IO]] = EmberClientBuilder
    .default[IO]
    .build
    .flatMap { c =>
      SimpleRestJsonBuilder(service).clientResource(c, uri"http://localhost:8082")
    }
    .map(new Runner(service, _))

}
