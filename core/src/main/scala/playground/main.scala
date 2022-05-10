package playground

import cats.Defer
import cats.Id
import cats.MonadThrow
import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.effect.implicits._
import cats.effect.Async
import cats.implicits._
import cats.~>
import org.http4s.Uri
import org.http4s.client.Client
import playground._
import playground.smithyql.InputNode
import playground.smithyql.OperationName
import playground.smithyql.Query
import playground.smithyql.WithSource
import smithy4s.Endpoint
import smithy4s.Service
import smithy4s.aws.AwsCall
import smithy4s.aws.AwsClient
import smithy4s.aws.AwsEnvironment
import smithy4s.aws.AwsOperationKind
import smithy4s.aws.http4s.AwsHttp4sBackend
import smithy4s.aws.kernel.AwsRegion
import smithy4s.http4s.SimpleRestJsonBuilder
import aws.protocols.AwsJson1_0
import aws.protocols.AwsJson1_1
import smithy4s.http4s.SimpleProtocolBuilder
import smithy4s.ShapeId
import cats.kernel.Semigroup
import smithy4s.dynamic.DynamicSchemaIndex
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.UseClause
import org.http4s.implicits._
import cats.effect.std

trait CompiledInput {
  type _Op[_, _, _, _, _]
  type I
  type E
  type O
  def input: I
  def catchError: Throwable => Option[E]
  def writeError: Option[NodeEncoder[E]]
  def writeOutput: NodeEncoder[O]
  def endpoint: Endpoint[_Op, I, E, O, _, _]
}

object CompiledInput {

  type Aux[_I, _E, _O, Op[_, _, _, _, _]] =
    CompiledInput {
      type _Op[I, E, O, SE, SO] = Op[I, E, O, SE, SO]
      type I = _I
      type E = _E
      type O = _O
    }

}

trait Compiler[F[_]] { self =>
  def compile(q: Query[WithSource]): F[CompiledInput]

  def mapK[G[_]](fk: F ~> G): Compiler[G] =
    new Compiler[G] {
      def compile(q: Query[WithSource]): G[CompiledInput] = fk(self.compile(q))
    }

}

object Compiler {

  def fromSchemaIndex[F[_]: MonadThrow](
    dsi: DynamicSchemaIndex
  ): Compiler[F] = new CompilerImpl(dsi)

  def fromService[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _], F[_]: MonadThrow](
    service: Service[Alg, Op]
  ): Compiler[F] = new ServiceCompiler(service)

}

final case class CompilationFailed(errors: NonEmptyList[CompilationError]) extends Throwable

object CompilationFailed {
  def one(e: CompilationError): CompilationFailed = CompilationFailed(NonEmptyList.one(e))
}

private class ServiceCompiler[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _], F[_]: MonadThrow](
  service: Service[Alg, Op]
) extends Compiler[F] {

  private def compileEndpoint[In, Err, Out](
    e: Endpoint[Op, In, Err, Out, _, _]
  ): WithSource[InputNode[WithSource]] => F[CompiledInput] = {
    val inputCompiler = e.input.compile(QueryCompilerSchematic)
    val outputEncoder = e.output.compile(NodeEncoderSchematic)
    val errorEncoder = e.errorable.map(e => e.error.compile(NodeEncoderSchematic))

    ast =>
      inputCompiler
        .compile(ast)
        .toEither
        .leftMap(_.toNonEmptyList)
        .leftMap(CompilationFailed(_))
        .liftTo[F]
        .map { compiled =>
          new CompiledInput {
            type _Op[_I, _E, _O, _SE, _SO] = Op[_I, _E, _O, _SE, _SO]
            type I = In
            type E = Err
            type O = Out
            val input: I = compiled
            val endpoint: Endpoint[Op, I, E, O, _, _] = e
            val writeOutput: NodeEncoder[Out] = outputEncoder
            val writeError: Option[NodeEncoder[Err]] = errorEncoder
            val catchError: Throwable => Option[Err] = err => e.errorable.flatMap(_.liftError(err))
          }
        }
  }

  private val endpoints = service
    .endpoints
    .groupByNel(_.name)
    .map(_.map(_.head).map(compileEndpoint(_)))

  def compile(q: Query[WithSource]): F[CompiledInput] = endpoints
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

private class CompilerImpl[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _], F[_]: MonadThrow](
  dsi: DynamicSchemaIndex
) extends Compiler[F] {

  private val services: Map[QualifiedIdentifier, Compiler[F]] =
    dsi
      .allServices
      .map { svc =>
        QualifiedIdentifier
          .fromShapeId(svc.service.id) -> Compiler.fromService[svc.Alg, svc.Op, F](svc.service)
      }
      .toMap

  private def getService(useClause: Option[WithSource[UseClause]]): F[Compiler[F]] =
    useClause match {

      // todo validate only one service if no use clause
      case None => services.head._2.pure[F]
      case Some(clause) =>
        services
          .get(clause.value.identifier)
          .liftTo[F](
            CompilationFailed.one(
              CompilationError(
                CompilationErrorDetails
                  .UnknownService(clause.value.identifier, services.keySet.toList),
                clause.range,
              )
            )
          )
    }

  def compile(
    q: Query[WithSource]
  ): F[CompiledInput] = getService(q.useClause).flatMap(_.compile(q))

}

trait Runner[F[_]] {
  def run(q: CompiledInput): F[InputNode[Id]]
}

object Runner {

  trait Optional[F[_]] {
    def get: Either[Issue, Runner[F]]
  }

  sealed trait Issue extends Product with Serializable

  object Issue {
    final case class InvalidProtocols(supported: NonEmptyList[ShapeId]) extends Issue
    final case class Other(e: Throwable) extends Issue

    // trust me, lawful
    implicit val semigroup: Semigroup[Issue] =
      (a, b) =>
        (a, b) match {
          case (e: Other, _)                                => e
          case (_, e: Other)                                => e
          case (InvalidProtocols(p1), InvalidProtocols(p2)) => InvalidProtocols(p1 |+| p2)
        }

  }

  def dynamicBaseUri[F[_]: MonadCancelThrow](getUri: F[Uri]): Client[F] => Client[F] =
    client =>
      Client[F] { req =>
        getUri.toResource.flatMap { uri =>
          client.run(
            req.withUri(
              req
                .uri
                .copy(
                  scheme = uri.scheme,
                  authority = uri.authority,
                  // prefixing with uri.path
                  path = uri.path.addSegments(req.uri.path.segments),
                )
            )
          )
        }
      }

  def make[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _], F[_]: Async: std.Console](
    service: Service[Alg, Op],
    client: Client[F],
    baseUri: F[Uri],
  ): Resource[F, Optional[F]] =
    // todo: configurable region
    AwsEnvironment
      .default(AwsHttp4sBackend(client), AwsRegion.US_EAST_1)
      .memoize
      .map { awsEnv =>
        new Optional[F] {

          private def simpleFromBuilder(
            builder: SimpleProtocolBuilder[_]
          ) = Either
            .catchNonFatal {
              builder(service).client(
                dynamicBaseUri[F](
                  baseUri.flatTap { uri =>
                    std.Console[F].println(s"Using base URI: $uri")
                  }
                ).apply(client),
                // this will be overridden by the middleware
                uri"http://example.com",
              )
            }
            .leftMap(Issue.Other(_))
            .flatMap {
              _.leftMap(e => Issue.InvalidProtocols(NonEmptyList.one(e.protocolTag.id)))
            }
            .map(service.asTransformation)

          // todo: upstream this. Get an AwsClient variant that can be statically used on a service.
          val awsInterpreter: Either[Issue, smithy4s.Interpreter[Op, F]] = service
            .hints
            .get(AwsJson1_0)
            .toRight(AwsJson1_0)
            .orElse(
              service
                .hints
                .get(AwsJson1_1)
                .toRight(AwsJson1_1)
            )
            .void
            .as {
              liftInterpreterResource(
                awsEnv
                  .flatMap(AwsClient(service, _))
                  .map(flattenAwsInterpreter(_, service))
              )
            }
            .leftMap(_ => Issue.InvalidProtocols(NonEmptyList.of(AwsJson1_0.id, AwsJson1_1.id)))

          private def perform[I, E, O, Op[_, _, _, _, _]](
            interpreter: smithy4s.Interpreter[Op, F],
            q: CompiledInput.Aux[I, E, O, Op],
          ) = Defer[F].defer(interpreter(q.endpoint.wrap(q.input))).map { response =>
            q.writeOutput.toNode(response)
          }

          val get: Either[Issue, Runner[F]] = NonEmptyList
            .of(
              simpleFromBuilder(SimpleRestJsonBuilder),
              awsInterpreter,
            )
            // orElse with error accumulation
            .reduceMapK(_.toValidated)
            .toEither
            .map { interpreter => q =>
              // todo: runner needs to support multiple services too, for now picking one
              perform[q.I, q.E, q.O, Op](
                interpreter,
                q.asInstanceOf[CompiledInput.Aux[q.I, q.E, q.O, Op]],
              )
            }

        }
      }

  def flattenAwsInterpreter[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _], F[_]](
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

  def liftInterpreterResource[Op[_, _, _, _, _], F[_]: MonadCancelThrow](
    interpreterR: Resource[F, smithy4s.Interpreter[Op, F]]
  ): smithy4s.Interpreter[Op, F] =
    new smithy4s.Interpreter[Op, F] {
      def apply[I, E, O, SI, SO](fa: Op[I, E, O, SI, SO]): F[O] = interpreterR.use(_.apply(fa))
    }

}
