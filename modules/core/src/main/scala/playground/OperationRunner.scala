package playground

import aws.protocols.AwsJson1_0
import aws.protocols.AwsJson1_1
import cats.Defer
import cats.Id
import cats.data.IorNel
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.effect.implicits._
import cats.effect.std
import cats.implicits._
import org.http4s.Uri
import org.http4s.client.Client
import playground._
import playground.plugins.PlaygroundPlugin
import playground.smithyql.InputNode
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.Query
import playground.smithyql.WithSource
import playground.std.Stdlib
import playground.std.StdlibRuntime
import smithy.api.ProtocolDefinition
import smithy4s.Service
import smithy4s.ShapeId
import smithy4s.aws.AwsCall
import smithy4s.aws.AwsClient
import smithy4s.aws.AwsEnvironment
import smithy4s.aws.AwsOperationKind
import smithy4s.dynamic.DynamicSchemaIndex
import smithy4s.http4s.SimpleProtocolBuilder
import smithy4s.http4s.SimpleRestJsonBuilder
import smithy4s.schema.Schema
import smithy4s.kinds._
import smithyql.syntax._

trait OperationRunner[F[_]] {
  def run(q: CompiledInput): F[InputNode[Id]]
}

object OperationRunner {

  trait Resolver[F[_]] {
    def get(parsed: Query[WithSource]): IorNel[Issue, OperationRunner[F]]
  }

  sealed trait Issue extends Product with Serializable

  object Issue {
    final case class InvalidProtocol(supported: ShapeId, found: List[ShapeId]) extends Issue
    final case class Other(e: Throwable) extends Issue

    final case class ProtocolIssues(supported: NonEmptyList[ShapeId], found: List[ShapeId])

    // Either remove all protocol errors, or only keep those.
    // If there are any non-protocol errors, they'll be returned in Right.
    // If there are only protocol errors, they'll be returned in Left
    // todo: this needs a cleanup
    def squash(
      issues: NonEmptyList[Issue]
    ): Either[ProtocolIssues, NonEmptyList[Throwable]] = {
      // todo: use nonEmptyPartition
      val (protocols, others) = issues.toList.partitionMap {
        case InvalidProtocol(p, _) => Left(p)
        case Other(e)              => Right(e)
      }

      others.toNel match {
        case None =>
          // must be nonempty at this point
          ProtocolIssues(
            NonEmptyList.fromListUnsafe(protocols),
            issues
              .collectFirst { case InvalidProtocol(_, found) => found }
              .getOrElse(
                sys.error(
                  "Impossible - no protocol issues found, can't extract available protocols"
                )
              ),
          ).asLeft
        case Some(otherErrors) => otherErrors.asRight
      }
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

  def forSchemaIndex[F[_]: StdlibRuntime: Concurrent: Defer: std.Console](
    dsi: DynamicSchemaIndex,
    client: Client[F],
    baseUri: F[Uri],
    awsEnv: Resource[F, AwsEnvironment[F]],
    plugins: List[PlaygroundPlugin],
  ): Resolver[F] = {
    val runners: Map[QualifiedIdentifier, Resolver[F]] =
      dsi
        .allServices
        .map { svc =>
          QualifiedIdentifier.forService(svc.service) ->
            OperationRunner.forService[svc.Alg, svc.Op, F](
              svc.service,
              client,
              baseUri,
              awsEnv,
              dsi.getSchema,
              plugins,
            )
        }
        .toMap

    new Resolver[F] {
      def get(q: Query[WithSource]): IorNel[Issue, OperationRunner[F]] = MultiServiceResolver
        .resolveService(
          q.mapK(WithSource.unwrap).collectServiceIdentifiers,
          runners,
        )
        .leftMap(rf =>
          CompilationFailed.one(
            CompilationError.error(
              CompilationErrorDetails.fromResolutionFailure(rf),
              q.useClause.value.fold(q.operationName.range)(_.identifier.range),
            )
          )
        )
        .toIor
        .leftMap(Issue.Other(_))
        .toIorNel
        .flatMap(_.get(q))
    }
  }

  def forService[
    Alg[_[_, _, _, _, _]],
    Op[_, _, _, _, _],
    F[_]: StdlibRuntime: Concurrent: Defer: std.Console,
  ](
    service: Service[Alg, Op],
    client: Client[F],
    baseUri: F[Uri],
    awsEnv: Resource[F, AwsEnvironment[F]],
    schemaIndex: ShapeId => Option[Schema[_]],
    plugins: List[PlaygroundPlugin],
  ): Resolver[F] =
    new Resolver[F] {

      val serviceProtocols = service
        .hints
        .all
        .toList
        .flatMap { binding =>
          schemaIndex(binding.keyId).flatMap { schemaOfHint =>
            schemaOfHint.hints.get(ProtocolDefinition).as(binding.keyId)
          }
        }

      private def simpleFromBuilder(
        builder: SimpleProtocolBuilder[_]
      ): IorNel[Issue, FunctorInterpreter[Op, F]] =
        Either
          .catchNonFatal {
            builder(service)
              .client(
                dynamicBaseUri[F](
                  baseUri.flatTap { uri =>
                    std.Console[F].println(s"Using base URI: $uri")
                  }
                ).apply(client)
              )
              .use
          }
          .leftMap(Issue.Other(_))
          .flatMap {
            _.leftMap(e => Issue.InvalidProtocol(e.protocolTag.id, serviceProtocols))
          }
          .map(service.toPolyFunction)
          .toIor
          .toIorNel

      private def stdlibRunner: IorNel[Issue, FunctorInterpreter[Op, F]] =
        smithy4s
          .checkProtocol(
            service,
            Stdlib.getTag,
          )
          .leftMap(e => Issue.InvalidProtocol(e.protocolTag.id, serviceProtocols): Issue)
          .toIor
          .toIorNel *> {
          val proxy = new DynamicServiceProxy[Alg, Op](service)

          proxy
            .tryProxy(StdlibRuntime[F].random)
            .orElse(proxy.tryProxy(StdlibRuntime[F].clock))
            .toRightIor(Issue.Other(new Throwable("unknown standard service")))
            .toIorNel
        }

      val awsFunctorInterpreter: IorNel[Issue, FunctorInterpreter[Op, F]] = AwsClient
        .prepare(service)
        .as {
          liftFunctorInterpreterResource(
            awsEnv
              .flatMap(AwsClient(service, _))
              .map(flattenAwsFunctorInterpreter(_, service))
          )
        }
        .toIor
        .leftMap(_ =>
          NonEmptyList
            .of(AwsJson1_0.id, AwsJson1_1.id)
            .map(Issue.InvalidProtocol(_, serviceProtocols))
        )

      private def perform[I, E, O](
        FunctorInterpreter: FunctorInterpreter[Op, F],
        q: CompiledInput.Aux[I, E, O, Op],
      ) = Defer[F].defer(FunctorInterpreter(q.wrap(q.input))).map { response =>
        q.writeOutput.toNode(response)
      }

      val runners: NonEmptyList[IorNel[Issue, OperationRunner[F]]] = NonEmptyList
        .of(
          simpleFromBuilder(SimpleRestJsonBuilder),
          awsFunctorInterpreter,
        )
        .concat(plugins.flatMap(_.http4sBuilders).map(simpleFromBuilder))
        .append(stdlibRunner)
        .map(
          _.map { FunctorInterpreter =>
            new OperationRunner[F] {
              def run(q: CompiledInput): F[InputNode[Id]] = perform[q.I, q.E, q.O](
                FunctorInterpreter,
                // note: this is safe... for real
                q.asInstanceOf[CompiledInput.Aux[q.I, q.E, q.O, Op]],
              )
            }
          }
        )

      val getInternal: IorNel[Issue, OperationRunner[F]] = runners.reduce(
        IorUtils.orElseCombine[NonEmptyList[Issue], OperationRunner[F]]
      )

      def get(parsed: Query[WithSource]): IorNel[Issue, OperationRunner[F]] = getInternal
    }

  def flattenAwsFunctorInterpreter[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _], F[_]](
    alg: AwsClient[Alg, F],
    service: Service[Alg, Op],
  ): FunctorInterpreter[Op, F] = service
    .toPolyFunction(alg)
    .andThen(new FunctorInterpreter[AwsCall[F, *, *, *, *, *], F] {

      def apply[I, E, O, SI, SO](
        fa: AwsCall[F, I, E, O, SI, SO]
      ): F[O] =
        // todo big hack!
        fa.run(AwsOperationKind.Unary.unary.asInstanceOf[AwsOperationKind.Unary[SI, SO]])

    })

  def liftFunctorInterpreterResource[Op[_, _, _, _, _], F[_]: MonadCancelThrow](
    FunctorInterpreterR: Resource[F, FunctorInterpreter[Op, F]]
  ): FunctorInterpreter[Op, F] =
    new FunctorInterpreter[Op, F] {

      def apply[I, E, O, SI, SO](
        fa: Op[I, E, O, SI, SO]
      ): F[O] = FunctorInterpreterR.use(_.apply(fa))

    }

}
