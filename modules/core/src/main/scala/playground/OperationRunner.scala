package playground

import cats.Defer
import cats.Id
import cats.data.Ior
import cats.data.IorNel
import cats.data.NonEmptyList
import cats.effect.Async
import cats.kernel.Semigroup
import cats.syntax.all.*
import fs2.compression.Compression
import jsonrpclib.JsonRPC
import jsonrpclib.fs2.*
import org.http4s.Uri
import org.http4s.client.Client
import playground.plugins.Interpreter
import playground.plugins.PlaygroundPlugin
import playground.plugins.SimpleHttpBuilder
import playground.smithyql.InputNode
import playground.smithyql.Prelude
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.Query
import playground.smithyql.WithSource
import smithy.api.ProtocolDefinition
import smithy4s.Service
import smithy4s.ShapeId
import smithy4s.dynamic.DynamicSchemaIndex
import smithy4s.kinds.*
import smithy4s.schema.Schema
import smithy4sbsp.bsp4s.BSPCodecs
import smithyql.syntax.*

trait OperationRunner[F[_]] {

  def run(
    q: CompiledInput
  ): F[InputNode[Id]]

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

    private def findProtocols[Alg[_[_, _, _, _, _]]](
      service: Service[Alg],
      schemaIndex: ShapeId => Option[Schema[?]],
    ) = service
      .hints
      .all
      .toList
      .flatMap { binding =>
        schemaIndex(binding.keyId).flatMap { schemaOfHint =>
          schemaOfHint.hints.get[ProtocolDefinition].as(binding.keyId)
        }
      }

    def fromPluginIssue[Alg[_[_, _, _, _, _]]](
      i: Interpreter.Issue,
      service: Service[Alg],
      schemaIndex: ShapeId => Option[Schema[?]],
    ): Issue =
      i match {
        case Interpreter.Issue.InvalidProtocol(supported) =>
          InvalidProtocol(supported, findProtocols(service, schemaIndex))
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

  def forServices[F[_]: Async](
    services: List[DynamicSchemaIndex.ServiceWrapper],
    getSchema: ShapeId => Option[Schema[?]],
    interpreters: NonEmptyList[Interpreter[F]],
  ): Map[QualifiedIdentifier, Resolver[F]] =
    services.map { svc =>
      QualifiedIdentifier.forService(svc.service) ->
        OperationRunner.forService[svc.Alg, F](
          svc.service,
          getSchema,
          interpreters,
        )
    }.toMap

  private def forService[Alg[_[_, _, _, _, _]], F[_]: Async](
    service: Service[Alg],
    schemaIndex: ShapeId => Option[Schema[?]],
    interpreters: NonEmptyList[Interpreter[F]],
  ): Resolver[F] =
    new {

      private def perform[E, O](
        interpreter: FunctorInterpreter[service.Operation, F],
        q: CompiledInput.Aux[E, O, service.Operation],
      ) = Defer[F].defer(interpreter(q.op)).map { response =>
        q.writeOutput.toNode(response)
      }

      val convertIssue = Issue.fromPluginIssue[Alg](_, service, schemaIndex)

      val runners: NonEmptyList[IorNel[Issue, OperationRunner[F]]] = interpreters
        .map(
          _.provide(service, schemaIndex)
            .leftMap(_.map(convertIssue))
        )
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
