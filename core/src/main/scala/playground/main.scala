package playground

import cats.FlatMap
import cats.MonadThrow
import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.implicits._
import cats.~>
import org.http4s.HttpApp
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.implicits._
import playground._
import playground.smithyql.AST
import playground.smithyql.Query
import playground.smithyql.WithSource
import smithy4s.Endpoint
import smithy4s.Service
import smithy4s.http4s.SimpleRestJsonBuilder
import playground.smithyql.OperationName

trait CompiledInput[Op[_, _, _, _, _]] {
  type I
  def input: I
  def endpoint: Endpoint[Op, I, _, _, _, _]
}

trait Compiler[Op[_, _, _, _, _], F[_]] { self =>
  def compile(q: Query[WithSource]): F[CompiledInput[Op]]

  def mapK[G[_]](fk: F ~> G): Compiler[Op, G] =
    new Compiler[Op, G] {
      def compile(q: Query[WithSource]): G[CompiledInput[Op]] = fk(self.compile(q))
    }

}

object Compiler {

  def instance[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _], F[_]: MonadThrow](
    service: Service[Alg, Op]
  ): Compiler[Op, F] = new CompilerImpl(service)

}

final case class CompilationFailed(errors: NonEmptyList[CompilationError]) extends Throwable

final case class OperationNotFound(
  name: WithSource[OperationName],
  validOperations: List[OperationName],
) extends Throwable

private class CompilerImpl[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _], F[_]: MonadThrow](
  service: Service[Alg, Op]
) extends Compiler[Op, F] {

  private val schem = new QueryCompilerSchematic

  // for quick lookup and prepared compilers
  private val endpoints: Map[String, AST[WithSource] => F[CompiledInput[Op]]] = {
    def go[In](
      e: Endpoint[Op, In, _, _, _, _]
    ): AST[WithSource] => F[CompiledInput[Op]] = {
      val schematic = e.input.compile(schem)

      ast =>
        schematic
          .compile(ast)
          .toEither
          .leftMap(_.toNonEmptyList)
          .leftMap(CompilationFailed(_))
          .liftTo[F]
          .map { compiled =>
            new CompiledInput[Op] {
              type I = In
              val input: I = compiled
              val endpoint: Endpoint[Op, I, _, _, _, _] = e
            }
          }
    }

    service
      .endpoints
      .groupByNel(_.name)
      .map(_.map(_.head).map(go(_)))
  }

  def compile(q: Query[WithSource]): F[CompiledInput[Op]] = endpoints
    .get(q.operationName.value.text)
    .liftTo[F](
      OperationNotFound(q.operationName, endpoints.keys.map(OperationName(_)).toList)
    )
    .flatMap(_.apply(q.input))

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
