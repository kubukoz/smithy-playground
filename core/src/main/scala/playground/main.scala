package playground

import cats.FlatMap
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
import playground.AST._

trait CompiledInput[Op[_, _, _, _, _]] {
  type I
  def input: I
  def endpoint: Endpoint[Op, I, _, _, _, _]
}

trait Compiler[Op[_, _, _, _, _], F[_]] { self =>
  def compile(q: Query): F[CompiledInput[Op]]
}

object Compiler {

  def instance[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
    service: Service[Alg, Op]
  ): Compiler[Op, Id] = new CompilerImpl(service)

}

private class CompilerImpl[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
  service: Service[Alg, Op]
) extends Compiler[Op, Id] {

  private val schem = new QuerySchematic

  // for quick lookup and decoding from AST
  private val endpoints: Map[String, AST => CompiledInput[Op]] = {
    def go[In](
      e: Endpoint[Op, In, _, _, _, _]
    ): AST => CompiledInput[Op] = {
      val schematic = e.input.compile(schem)

      ast =>
        new CompiledInput[Op] {
          type I = In
          val input: I = schematic(ast)
          val endpoint: Endpoint[Op, I, _, _, _, _] = e
        }
    }

    service
      .endpoints
      .groupByNel(_.name)
      .map(_.map(_.head).map(go(_)))
  }

  def compile(q: Query): CompiledInput[Op] =
    endpoints.getOrElse(
      q.operationName,
      throw new Exception(
        s"Operation not found: ${q.operationName}. Available operations: ${endpoints.keys.mkString(", ")}"
      ),
    )(q.input)

}

trait Runner[F[_], Op[_, _, _, _, _]] {
  def run(q: CompiledInput[Op]): F[Any]
}

object Runner {

  def unlift[F[_]: FlatMap, Op[_, _, _, _, _]](runner: F[Runner[F, Op]]): Runner[F, Op] =
    new Runner[F, Op] {
      def run(q: CompiledInput[Op]): F[Any] = runner.flatMap(_.run(q))
    }

  def make[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _], F[_]](
    service: Service[Alg, Op],
    impl: Option[HttpApp[IO]] = None,
  ): Resource[IO, Runner[IO, Op]] = EmberClientBuilder
    .default[IO]
    .build
    .flatMap { c =>
      val b = SimpleRestJsonBuilder(service)

      impl
        .fold(
          b.clientResource(c, uri"http://localhost:8082")
        )(b.clientResource(_, uri"http://localhost:8082"))
        .map { client =>
          val exec = service.asTransformation(client)

          q => IO.defer(exec(q.endpoint.wrap(q.input)))
        }
    }

}