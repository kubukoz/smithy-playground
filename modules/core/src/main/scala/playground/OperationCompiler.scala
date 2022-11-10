package playground

import cats.Id
import cats.data.Ior
import cats.data.IorNel
import cats.data.NonEmptyList
import cats.effect.implicits._
import cats.implicits._
import cats.~>
import playground._
import playground.smithyql.InputNode
import playground.smithyql.OperationName
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.Query
import playground.smithyql.WithSource
import smithy.api
import smithy4s.Service
import smithy4s.dynamic.DynamicSchemaIndex

import smithyql.syntax._
import types._

trait CompiledInput {
  type _Op[_, _, _, _, _]
  type I
  type E
  type O
  def input: I
  def catchError: Throwable => Option[E]
  def writeError: Option[NodeEncoder[E]]
  def writeOutput: NodeEncoder[O]
  def serviceId: QualifiedIdentifier
  def wrap(i: I): _Op[I, E, O, _, _]
}

object CompiledInput {

  type Aux[_I, _E, _O, Op[_, _, _, _, _]] =
    CompiledInput {
      type _Op[__I, __E, __O, __SE, __SO] = Op[__I, __E, __O, __SE, __SO]
      type I = _I
      type E = _E
      type O = _O
    }

}

trait OperationCompiler[F[_]] { self =>
  def compile(q: Query[WithSource]): F[CompiledInput]

  def mapK[G[_]](fk: F ~> G): OperationCompiler[G] =
    new OperationCompiler[G] {
      def compile(q: Query[WithSource]): G[CompiledInput] = fk(self.compile(q))
    }

}

object OperationCompiler {

  def fromSchemaIndex(
    dsi: DynamicSchemaIndex
  ): OperationCompiler[Ior[Throwable, *]] = {
    val services: Map[QualifiedIdentifier, OperationCompiler[Ior[Throwable, *]]] =
      dsi
        .allServices
        .map { svc =>
          QualifiedIdentifier
            .forService(svc.service) -> OperationCompiler.fromService[svc.Alg](
            svc.service
          )
        }
        .toMap

    new MultiServiceCompiler(services)
  }

  def fromService[Alg[_[_, _, _, _, _]]](
    service: Service[Alg]
  ): OperationCompiler[Ior[Throwable, *]] = new ServiceCompiler(service)

}

final case class CompilationFailed(errors: NonEmptyList[CompilationError]) extends Throwable

object CompilationFailed {
  def one(e: CompilationError): CompilationFailed = CompilationFailed(NonEmptyList.one(e))
}

private class ServiceCompiler[Alg[_[_, _, _, _, _]]](
  service: Service[Alg]
) extends OperationCompiler[Ior[Throwable, *]] {

  private def compileEndpoint[In, Err, Out](
    e: service.Endpoint[In, Err, Out, _, _]
  ): WithSource[InputNode[WithSource]] => IorNel[CompilationError, CompiledInput] = {
    val inputCompiler = e.input.compile(QueryCompilerVisitor.full)
    val outputEncoder = NodeEncoder.derive(e.output)
    val errorEncoder = e.errorable.map(e => NodeEncoder.derive(e.error))

    ast =>
      inputCompiler
        .compile(ast)
        .leftMap(_.toNonEmptyList)
        .map { compiled =>
          new CompiledInput {
            type _Op[_I, _E, _O, _SE, _SO] = service.Operation[_I, _E, _O, _SE, _SO]
            type I = In
            type E = Err
            type O = Out
            val input: I = compiled
            val serviceId: QualifiedIdentifier = QualifiedIdentifier.forService(service)

            def wrap(i: I): _Op[I, E, O, _, _] = e.wrap(i)
            val writeOutput: NodeEncoder[O] = outputEncoder
            val writeError: Option[NodeEncoder[E]] = errorEncoder
            val catchError: Throwable => Option[E] = err => e.errorable.flatMap(_.liftError(err))
          }
        }
  }

  private val endpoints = service
    .endpoints
    .groupByNel(_.name)
    .map(_.map(_.head).map(e => (e, compileEndpoint(e))))

  private def operationNotFound(q: Query[WithSource]): CompilationError = CompilationError.error(
    CompilationErrorDetails
      .OperationNotFound(
        q.operationName.value.operationName.value.mapK(WithSource.unwrap),
        endpoints.keys.map(OperationName[Id](_)).toList,
      ),
    q.operationName.range,
  )

  private def deprecationWarnings(q: Query[WithSource]) =
    q.useClause.value match {
      // If the use clause is present, in normal flow it's 100% safe to assume that it matches this compiler's service.
      case Some(useClause) =>
        service
          .hints
          .get(api.Deprecated)
          .map { info =>
            CompilationError.deprecation(info, useClause.identifier.range)
          }
          .toBothLeft(())
          .toIorNel

      case None => Ior.right(())
    }

  private def deprecatedOperationCheck(
    q: Query[WithSource],
    endpoint: service.Endpoint[_, _, _, _, _],
  ) =
    endpoint
      .hints
      .get(api.Deprecated)
      .map { info =>
        CompilationError.deprecation(info, q.operationName.range)
      }
      .toBothLeft(())
      .toIorNel

  private def seal[A](
    result: IorNel[CompilationError, A]
  ): IorNel[CompilationError, A] = result.fold(
    Ior.left(_),
    Ior.right(_),
    (e, a) =>
      if (e.exists(_.isError))
        Ior.left(e)
      else
        Ior.both(e, a),
  )

  def compile(q: Query[WithSource]): Ior[Throwable, CompiledInput] = {
    val compiled =
      endpoints
        .get(q.operationName.value.operationName.value.text)
        .toRightIor(NonEmptyList.one(operationNotFound(q)))
        .flatTap { case (e, _) => deprecatedOperationCheck(q, e) }
        .flatMap(_._2.apply(q.input)) <& deprecationWarnings(q)

    seal(compiled).leftMap(CompilationFailed(_))
  }

}

private class MultiServiceCompiler[Alg[_[_, _, _, _, _]]](
  services: Map[QualifiedIdentifier, OperationCompiler[Ior[Throwable, *]]]
) extends OperationCompiler[Ior[Throwable, *]] {

  private def getService(
    q: Query[WithSource]
  ): Either[Throwable, OperationCompiler[Ior[Throwable, *]]] = MultiServiceResolver
    .resolveService(
      q.mapK(WithSource.unwrap).collectServiceIdentifiers,
      services,
    )
    .leftMap { rf =>
      CompilationFailed.one(
        ResolutionFailure.toCompilationError(rf, q)
      )
    }

  def compile(
    q: Query[WithSource]
  ): Ior[Throwable, CompiledInput] = getService(q).fold(Ior.left(_), _.compile(q))

}
