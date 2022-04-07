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
import cats.effect.kernel.Async
import smithy4s.aws.kernel.AwsRegion
import smithy4s.aws.AwsClient
import smithy4s.Transformation
import smithy4s.aws.AwsCall
import smithy4s.aws.AwsEnvironment
import smithy4s.aws.http4s.AwsHttp4sBackend
import cats.effect.implicits._
import cats.effect.Resource
import cats.effect.MonadCancelThrow
import smithy4s.aws.AwsOperationKind

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
    def get: Either[Issue, Runner[F, Op]]
  }

  sealed trait Issue extends Product with Serializable

  object Issue {
    final case class InvalidProtocol(e: UnsupportedProtocolError) extends Issue
    final case class Other(e: Throwable) extends Issue
  }

  def make[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _], F[_]: Async](
    service: Service[Alg, Op],
    client: Client[F],
    baseUri: Uri,
  ): Resource[F, Optional[F, Op]] =
    // todo: configurable region
    AwsEnvironment
      .default(AwsHttp4sBackend(client), AwsRegion.US_EAST_1)
      .memoize
      .map { awsEnv =>
        new Optional[F, Op] {

          val xa: smithy4s.Interpreter[Op, F] = liftMagic(
            awsEnv
              .flatMap(AwsClient.resource(service, _))
              .map(magic(_, service))
          )

          val get: Either[Issue, Runner[F, Op]] = Either
            .catchNonFatal {
              SimpleRestJsonBuilder(service).client(client, baseUri)
            }
            .leftMap(Issue.Other(_))
            .flatMap(_.leftMap(Issue.InvalidProtocol(_)))
            .map(service.asTransformation)
            // todo: this takes precedence now, probably not the best idea
            .orElse(Right(xa))
            .map { interpreter => q =>
              Defer[F].defer(interpreter(q.endpoint.wrap(q.input))).map { response =>
                q.writeOutput.toNode(response)
              }
            }

        }
      }

  def magic[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _], F[_]](
    alg: AwsClient[Alg, F],
    service: Service[Alg, Op],
  ): smithy4s.Interpreter[Op, F] = service
    .asTransformation(alg)
    .andThen(new smithy4s.Interpreter[AwsCall[F, *, *, *, *, *], F] {

      def apply[I, E, O, SI, SO](
        fa: AwsCall[F, I, E, O, SI, SO]
      ): F[O] =
        // todo big hack!
        fa.run(AwsOperationKind.Unary.unary.asInstanceOf[AwsOperationKind.Unary[SI, SO]])

    })

  def liftMagic[Op[_, _, _, _, _], F[_]: MonadCancelThrow](
    interpreterR: Resource[F, smithy4s.Interpreter[Op, F]]
  ): smithy4s.Interpreter[Op, F] =
    new smithy4s.Interpreter[Op, F] {
      def apply[I, E, O, SI, SO](fa: Op[I, E, O, SI, SO]): F[O] = interpreterR.use(_.apply(fa))
    }

}
