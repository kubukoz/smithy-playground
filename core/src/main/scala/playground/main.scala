package playground

import cats.Defer
import cats.Id
import cats.MonadThrow
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.implicits._
import cats.~>
import org.http4s.client.Client
import playground._
import playground.smithyql.InputNode
import playground.smithyql.OperationName
import playground.smithyql.Query
import playground.smithyql.WithSource
import smithy4s.Endpoint
import smithy4s.Service
import smithy4s.http4s.SimpleRestJsonBuilder
import smithy4s.UnsupportedProtocolError
import org.http4s.Uri

trait CompiledInput[Op[_, _, _, _, _]] {
  type I
  type E
  type O
  def input: I
  def catchError: Throwable => Option[E]
  def writeError: Option[NodeEncoder[E]]
  def writeOutput: NodeEncoder[O]
  def endpoint: Endpoint[Op, I, E, O, _, _]
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

object CompilationFailed {
  def one(e: CompilationError): CompilationFailed = CompilationFailed(NonEmptyList.one(e))
}

private class CompilerImpl[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _], F[_]: MonadThrow](
  service: Service[Alg, Op]
) extends Compiler[Op, F] {

  private val schem = new QueryCompilerSchematic

  // for quick lookup and prepared compilers
  private val endpoints: Map[String, WithSource[InputNode[WithSource]] => F[CompiledInput[Op]]] = {
    def go[In, Err, Out](
      e: Endpoint[Op, In, Err, Out, _, _]
    ): WithSource[InputNode[WithSource]] => F[CompiledInput[Op]] = {
      val schematic = e.input.compile(schem)
      val outputEncoder = e.output.compile(NodeEncoderSchematic)
      val errorEncoder = e.errorable.map(e => e.error.compile(NodeEncoderSchematic))

      ast =>
        schematic
          .compile(ast)
          .get
          .toEither
          .leftMap(_.toNonEmptyList)
          .leftMap(CompilationFailed(_))
          .liftTo[F]
          .map { compiled =>
            new CompiledInput[Op] {
              type I = In
              type E = Err
              type O = Out
              val input: I = compiled
              val endpoint: Endpoint[Op, I, E, O, _, _] = e
              val writeOutput: NodeEncoder[Out] = outputEncoder
              val writeError: Option[NodeEncoder[Err]] = errorEncoder
              val catchError: Throwable => Option[Err] =
                err => e.errorable.flatMap(_.liftError(err))
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
      CompilationFailed.one(
        CompilationError(
          CompilationErrorDetails
            .OperationNotFound(
              q.operationName.value,
              endpoints.keys.map(OperationName(_)).toList,
            ),
          q.operationName.range,
        )
      )
    )
    .flatMap(_.apply(q.input))

}

trait Runner[F[_], Op[_, _, _, _, _]] {
  def run(q: CompiledInput[Op]): F[InputNode[Id]]
}

object Runner {

  trait Optional[F[_], Op[_, _, _, _, _]] {
    def get: Either[UnsupportedProtocolError, Runner[F, Op]]
  }

  def make[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _], F[_]: Defer: Concurrent](
    service: Service[Alg, Op],
    client: Client[F],
    baseUri: Uri,
  ): Optional[F, Op] =
    new Optional[F, Op] {

      val get: Either[UnsupportedProtocolError, Runner[F, Op]] = SimpleRestJsonBuilder(service)
        .client(client, baseUri)
        .map { c =>
          val exec = service.asTransformation(c)

          q =>
            Defer[F].defer(exec(q.endpoint.wrap(q.input))).map { response =>
              q.writeOutput.toNode(response)
            }
        }

    }

}
