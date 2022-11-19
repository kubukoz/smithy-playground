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
import playground.plugins.SimpleHttpBuilder
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
import smithy4s.http4s.SimpleRestJsonBuilder
import smithy4s.schema.Schema

import smithyql.syntax._
import playground.smithyql.Prelude

trait OperationRunner[F[_]] {
  def run(q: CompiledInput): F[InputNode[Id]]
}

object OperationRunner {

  trait Resolver[F[_]] {

    def get(
      parsed: Query[WithSource],
      prelude: Prelude[WithSource],
    ): IorNel[Issue, OperationRunner[F]]

  }

  sealed trait Issue extends Product with Serializable

  object Issue {
    final case class InvalidProtocol(supported: ShapeId, found: List[ShapeId]) extends Issue
    final case class Other(e: Throwable) extends Issue

    sealed trait Squashed extends Product with Serializable

    object Squashed {

      final case class ProtocolIssues(
        supported: NonEmptyList[ShapeId],
        found: List[ShapeId],
      ) extends Squashed

      final case class OtherIssues(exceptions: NonEmptyList[Throwable]) extends Squashed
    }

    // Either remove all protocol errors, or only keep those.
    // If there are any non-protocol errors, they'll be returned in Right.
    // If there are only protocol errors, they'll be returned in Left
    // this would be nice to clean up
    def squash(
      issues: NonEmptyList[Issue]
    ): Squashed = {
      val (protocols, others) = issues.toList.partitionMap {
        case InvalidProtocol(p, _) => Left(p)
        case Other(e)              => Right(e)
      }

      others.toNel match {
        case None =>
          // must be nonempty at this point
          Squashed.ProtocolIssues(
            NonEmptyList.fromListUnsafe(protocols),
            issues
              .collectFirst { case InvalidProtocol(_, found) => found }
              .getOrElse(
                sys.error(
                  "Impossible - no protocol issues found, can't extract available protocols"
                )
              ),
          )
        case Some(otherErrors) => Squashed.OtherIssues(otherErrors)
      }
    }

  }

  // https://github.com/kubukoz/smithy-playground/issues/158
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
  ): Map[QualifiedIdentifier, Resolver[F]] = forServices(
    services = dsi.allServices,
    getSchema = dsi.getSchema,
    client = client,
    baseUri = baseUri,
    awsEnv = awsEnv,
    plugins = plugins,
  )

  def forServices[F[_]: StdlibRuntime: Concurrent: Defer: std.Console](
    services: List[DynamicSchemaIndex.ServiceWrapper],
    getSchema: ShapeId => Option[Schema[_]],
    client: Client[F],
    baseUri: F[Uri],
    awsEnv: Resource[F, AwsEnvironment[F]],
    plugins: List[PlaygroundPlugin],
  ): Map[QualifiedIdentifier, Resolver[F]] =
    services.map { svc =>
      QualifiedIdentifier.forService(svc.service) ->
        OperationRunner.forService[svc.Alg, svc.Op, F](
          svc.service,
          client,
          baseUri,
          awsEnv,
          getSchema,
          plugins,
        )
    }.toMap

  def merge[F[_]](
    runners: Map[QualifiedIdentifier, Resolver[F]],
    serviceIndex: ServiceIndex,
  ): Resolver[F] =
    new Resolver[F] {

      def get(
        q: Query[WithSource],
        prelude: Prelude[WithSource],
      ): IorNel[Issue, OperationRunner[F]] = MultiServiceResolver
        .resolveService(
          queryOperationName = q.operationName.value,
          serviceIndex = serviceIndex,
          useClauses = prelude.useClauses.map(_.value),
        )
        .map(runners(_))
        .leftMap(CompilationFailed(_))
        .toIor
        .leftMap(Issue.Other(_))
        .toIorNel
        .flatMap(_.get(q, prelude))

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
        builder: SimpleHttpBuilder
      ): IorNel[Issue, smithy4s.Interpreter[Op, F]] =
        builder
          .client(
            service,
            dynamicBaseUri[F](
              baseUri.flatTap { uri =>
                std.Console[F].println(s"Using base URI: $uri")
              }
            ).apply(client),
          )
          .leftMap(e => Issue.InvalidProtocol(e.protocolTag.id, serviceProtocols))
          .map(service.asTransformation)
          .toIor
          .toIorNel

      private def stdlibRunner: IorNel[Issue, smithy4s.Interpreter[Op, F]] =
        smithy4s
          .checkProtocol(
            service,
            Stdlib.getTag,
          )
          .leftMap(e => Issue.InvalidProtocol(e.protocolTag.id, serviceProtocols): Issue)
          .toIor
          .toIorNel *> {
          val proxy = new DynamicServiceProxy[Alg, Op](service)

          NonEmptyList
            .of(
              proxy.tryProxy(StdlibRuntime[F].random),
              proxy.tryProxy(StdlibRuntime[F].clock),
            )
            .reduceK // orElse
            .toRightIor(Issue.Other(new Throwable("unknown standard service")))
            .toIorNel
        }

      val awsInterpreter: IorNel[Issue, smithy4s.Interpreter[Op, F]] = AwsClient
        .prepare(service)
        .as {
          liftInterpreterResource(
            awsEnv
              .flatMap(AwsClient(service, _))
              .map(flattenAwsInterpreter(_, service))
          )
        }
        .toIor
        .leftMap(_ =>
          NonEmptyList
            .of(AwsJson1_0.id, AwsJson1_1.id)
            .map(Issue.InvalidProtocol(_, serviceProtocols))
        )

      private def perform[E, O](
        interpreter: smithy4s.Interpreter[Op, F],
        q: CompiledInput.Aux[E, O, Op],
      ) = Defer[F].defer(interpreter(q.op)).map { response =>
        q.writeOutput.toNode(response)
      }

      val runners: NonEmptyList[IorNel[Issue, OperationRunner[F]]] = NonEmptyList
        .of(
          simpleFromBuilder(SimpleHttpBuilder.fromSimpleProtocolBuilder(SimpleRestJsonBuilder)),
          awsInterpreter,
        )
        .concat(plugins.flatMap(_.simpleBuilders).map(simpleFromBuilder))
        .append(stdlibRunner)
        .map(
          _.map { interpreter =>
            new OperationRunner[F] {
              def run(q: CompiledInput): F[InputNode[Id]] = perform[q.E, q.O](
                interpreter,
                // note: this is safe... for real
                q.asInstanceOf[CompiledInput.Aux[q.E, q.O, Op]],
              )
            }
          }
        )

      val getInternal: IorNel[Issue, OperationRunner[F]] = runners.reduce(
        IorUtils.orElseCombine[NonEmptyList[Issue], OperationRunner[F]]
      )

      def get(
        parsed: Query[WithSource],
        prelude: Prelude[WithSource],
      ): IorNel[Issue, OperationRunner[F]] = getInternal

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
