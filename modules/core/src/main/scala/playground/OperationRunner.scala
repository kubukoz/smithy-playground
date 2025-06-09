package playground

import aws.protocols.AwsJson1_0
import aws.protocols.AwsJson1_1
import aws.protocols.AwsQuery
import aws.protocols.Ec2Query
import aws.protocols.RestJson1
import aws.protocols.RestXml
import cats.Defer
import cats.Id
import cats.MonadThrow
import cats.data.IorNel
import cats.data.NonEmptyList
import cats.effect.Async
import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.effect.implicits.*
import cats.effect.std
import cats.kernel.Semigroup
import cats.syntax.all.*
import fs2.compression.Compression
import org.http4s.Uri
import org.http4s.client.Client
import playground.plugins.Environment
import playground.plugins.Interpreter
import playground.plugins.PlaygroundPlugin
import playground.plugins.SimpleHttpBuilder
import playground.smithyql.InputNode
import playground.smithyql.Prelude
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.Query
import playground.smithyql.WithSource
import playground.std.Stdlib
import playground.std.StdlibRuntime
import smithy4s.Service
import smithy4s.ShapeId
import smithy4s.aws.AwsClient
import smithy4s.aws.AwsEnvironment
import smithy4s.dynamic.DynamicSchemaIndex
import smithy4s.http4s.SimpleRestJsonBuilder
import smithy4s.kinds.*
import smithy4s.schema.Schema
import smithyql.syntax.*

trait OperationRunner[F[_]] {

  def run(
    q: CompiledInput
  ): F[InputNode[Id]]

}

object Interpreters {

  def stdlib[F[_]: StdlibRuntime: MonadThrow]: Interpreter[F] =
    new {
      def provide[Alg[_[_, _, _, _, _]]](
        service: Service[Alg],
        schemaIndex: ShapeId => Option[Schema[?]],
      ): IorNel[Interpreter.Issue, service.FunctorInterpreter[F]] =
        smithy4s
          .checkProtocol(
            service,
            Stdlib.getTag,
          )
          .leftMap { e =>
            Interpreter
              .Issue
              .InvalidProtocol(
                e.protocolTag.id,
                Interpreter.protocols(service, schemaIndex),
              )
          }
          .toIor
          .toIorNel *> {
          val proxy = new DynamicServiceProxy[Alg, service.Operation](service)

          NonEmptyList
            .of(
              proxy.tryProxy(StdlibRuntime[F].random),
              proxy.tryProxy(StdlibRuntime[F].clock),
            )
            .reduceK // orElse
            .toRightIor(Interpreter.Issue.Other(new Throwable("unknown standard service")))
            .toIorNel
        }
    }

  def aws[F[_]: Compression: Async](awsEnv: Resource[F, AwsEnvironment[F]]): Interpreter[F] =
    new {
      def provide[Alg[_[_, _, _, _, _]]](
        service: Service[Alg],
        schemaIndex: ShapeId => Option[Schema[?]],
      ): IorNel[Interpreter.Issue, service.FunctorInterpreter[F]] = AwsClient
        .prepare(service)
        .map { builder =>
          awsEnv
            .map(builder.build(_))
            .map(service.toPolyFunction(_))
        }
        .map(liftFunctorInterpreterResource(_))
        .toIor
        .leftMap(_ =>
          NonEmptyList
            .of(AwsJson1_0.id, AwsJson1_1.id, RestJson1.id, AwsQuery.id, RestXml.id, Ec2Query.id)
            .map(Interpreter.Issue.InvalidProtocol(_, Interpreter.protocols(service, schemaIndex)))
        )
    }

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

    def fromPluginIssue(i: Interpreter.Issue): Issue =
      i match {
        case Interpreter.Issue.InvalidProtocol(supported, found) =>
          InvalidProtocol(supported, found)
        case Interpreter.Issue.Other(e) => Other(e)
      }

    final case class InvalidProtocol(
      supported: ShapeId,
      found: List[ShapeId],
    ) extends Issue

    final case class Other(
      e: Throwable
    ) extends Issue

    sealed trait Squashed extends Product with Serializable

    object Squashed {

      final case class ProtocolIssues(
        supported: NonEmptyList[ShapeId],
        found: List[ShapeId],
      ) extends Squashed

      final case class OtherIssues(
        exceptions: NonEmptyList[Throwable]
      ) extends Squashed

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
  def dynamicBaseUri[F[_]: MonadCancelThrow](
    getUri: F[Uri]
  ): Client[F] => Client[F] =
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

  def forSchemaIndex[F[_]: StdlibRuntime: Async: Compression: std.Console](
    dsi: DynamicSchemaIndex,
    client: Client[F],
    baseUri: F[Uri],
    awsEnv: Resource[F, AwsEnvironment[F]],
    plugins: List[PlaygroundPlugin],
  ): Map[QualifiedIdentifier, Resolver[F]] = forServices(
    services = dsi.allServices.toList,
    getSchema = dsi.getSchema,
    client = client,
    baseUri = baseUri,
    awsEnv = awsEnv,
    plugins = plugins,
  )

  def forServices[F[_]: StdlibRuntime: Async: Compression: std.Console](
    services: List[DynamicSchemaIndex.ServiceWrapper],
    getSchema: ShapeId => Option[Schema[?]],
    client: Client[F],
    baseUri: F[Uri],
    awsEnv: Resource[F, AwsEnvironment[F]],
    plugins: List[PlaygroundPlugin],
  ): Map[QualifiedIdentifier, Resolver[F]] =
    services.map { svc =>
      QualifiedIdentifier.forService(svc.service) ->
        OperationRunner.forService[svc.Alg, F](
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

  def forService[Alg[_[_, _, _, _, _]], F[_]: StdlibRuntime: Async: Compression: std.Console](
    service: Service[Alg],
    client: Client[F],
    baseUri: F[Uri],
    awsEnv: Resource[F, AwsEnvironment[F]],
    schemaIndex: ShapeId => Option[Schema[?]],
    plugins: List[PlaygroundPlugin],
  ): Resolver[F] =
    new Resolver[F] {
      given Environment[F] =
        new {
          def getK(k: Environment.Key): Option[k.Value[F]] = {
            val yolo: Option[Any] =
              k match {
                case Environment.httpClient => Some(client)
                case Environment.baseUri    => Some(baseUri)
                case Environment.console    => Some(std.Console[F])
                case _                      => None
              }
            yolo.map(_.asInstanceOf[k.Value[F]])
          }

          def requireK(k: Environment.Key): k.Value[F] = getK(k).getOrElse(
            sys.error(s"Required key $k not found in Environment")
          )

        }

      private val interpreters = NonEmptyList
        .of(
          Interpreter.fromSimpleBuilder(
            SimpleHttpBuilder.fromSimpleProtocolBuilder(SimpleRestJsonBuilder)
          ),
          Interpreters.aws(awsEnv),
        )
        .concat(plugins.flatMap(_.interpreters))
        .append(Interpreters.stdlib[F])

      private def perform[E, O](
        interpreter: FunctorInterpreter[service.Operation, F],
        q: CompiledInput.Aux[E, O, service.Operation],
      ) = Defer[F].defer(interpreter(q.op)).map { response =>
        q.writeOutput.toNode(response)
      }

      val runners: NonEmptyList[IorNel[Issue, OperationRunner[F]]] = interpreters
        .map(_.provide(service, schemaIndex).leftMap(_.map(Issue.fromPluginIssue)))
        .map(
          _.map { interpreter =>
            new OperationRunner[F] {
              def run(
                q: CompiledInput
              ): F[InputNode[Id]] = perform[q.E, q.O](
                interpreter,
                // note: this is safe... for real
                q.asInstanceOf[CompiledInput.Aux[q.E, q.O, service.Operation]],
              )
            }
          }
        )

      val getInternal: IorNel[Issue, OperationRunner[F]] = runners.reduce(
        using Semigroup.instance(
          IorUtils.orElseCombine[NonEmptyList[Issue], OperationRunner[F]](_, _)
        )
      )

      def get(
        parsed: Query[WithSource],
        prelude: Prelude[WithSource],
      ): IorNel[Issue, OperationRunner[F]] = getInternal

    }

}

def liftFunctorInterpreterResource[Op[_, _, _, _, _], F[_]: MonadCancelThrow](
  fir: Resource[F, FunctorInterpreter[Op, F]]
): FunctorInterpreter[Op, F] =
  new FunctorInterpreter[Op, F] {

    def apply[I, E, O, SI, SO](
      fa: Op[I, E, O, SI, SO]
    ): F[O] = fir.use(_.apply(fa))

  }
