package playground

import aws.protocols.AwsJson1_0
import aws.protocols.AwsJson1_1
import aws.protocols.AwsQuery
import aws.protocols.Ec2Query
import aws.protocols.RestJson1
import aws.protocols.RestXml
import cats.MonadThrow
import cats.data.IorNel
import cats.data.NonEmptyList
import cats.effect.Async
import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.syntax.all.*
import fs2.compression.Compression
import playground.plugins.Interpreter
import playground.std.Stdlib
import playground.std.StdlibRuntime
import smithy4s.Service
import smithy4s.ShapeId
import smithy4s.aws.AwsClient
import smithy4s.aws.AwsEnvironment
import smithy4s.kinds.FunctorInterpreter
import smithy4s.schema.Schema

/** Standard implementations of interpreters. More interpreters can be provided by instances of
  * [[playground.plugins.PlaygroundPlugin]] (the plugin system).
  */
object Interpreters {

  def stdlib[F[_]: StdlibRuntime: MonadThrow]: Interpreter[F] =
    new {
      def provide[Alg[_[_, _, _, _, _]]](
        service: Service[Alg],
        schemaIndex: ShapeId => Option[Schema[?]],
      ): IorNel[Interpreter.Issue, service.FunctorInterpreter[F]] =
        smithy4s
          .checkProtocol(service, Stdlib)
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
        .leftMap { _ =>
          NonEmptyList
            .of(AwsJson1_0.id, AwsJson1_1.id, RestJson1.id, AwsQuery.id, RestXml.id, Ec2Query.id)
            .map(Interpreter.Issue.InvalidProtocol(_, Interpreter.protocols(service, schemaIndex)))
        }
    }

}

private def liftFunctorInterpreterResource[Op[_, _, _, _, _], F[_]: MonadCancelThrow](
  fir: Resource[F, FunctorInterpreter[Op, F]]
): FunctorInterpreter[Op, F] =
  new FunctorInterpreter[Op, F] {

    def apply[I, E, O, SI, SO](
      fa: Op[I, E, O, SI, SO]
    ): F[O] = fir.use(_.apply(fa))

  }
