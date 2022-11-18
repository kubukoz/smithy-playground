package playground

import cats.Id
import cats.data.EitherNel
import cats.data.Ior
import cats.data.IorNel
import cats.data.Kleisli
import cats.data.NonEmptyList
import cats.effect.implicits._
import cats.implicits._
import cats.~>
import playground._
import playground.smithyql.InputNode
import playground.smithyql.OperationName
import playground.smithyql.Prelude
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.Query
import playground.smithyql.WithSource
import smithy.api
import smithy4s.Endpoint
import smithy4s.Service
import smithy4s.dynamic.DynamicSchemaIndex

import smithyql.syntax._
import types._
import util.chaining._
import playground.smithyql.UseClause

trait CompiledInput {
  type _Op[_, _, _, _, _]
  type E
  type O
  def catchError: Throwable => Option[E]
  def writeError: Option[NodeEncoder[E]]
  def writeOutput: NodeEncoder[O]
  def op: _Op[_, E, O, _, _]
}

object CompiledInput {

  type Aux[_E, _O, Op[_, _, _, _, _]] =
    CompiledInput {
      type _Op[__I, __E, __O, __SE, __SO] = Op[__I, __E, __O, __SE, __SO]
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

  final case class Context(prelude: Prelude[WithSource])

  type EffF[F[_], A] = Kleisli[F, Context, A]
  type Eff[A] = EffF[IorNel[CompilationError, *], A]

  object Eff {
    def liftF[A](fa: IorNel[CompilationError, A]): Eff[A] = Kleisli.liftF(fa)
    val getContext: Eff[Context] = Kleisli.ask

    def perform(prelude: Prelude[WithSource]): Eff ~> IorThrow = Kleisli
      .liftFunctionK(CompilationFailed.wrapK)
      .andThen(Kleisli.applyK(Context(prelude)))

  }

  def fromSchemaIndex(
    dsi: DynamicSchemaIndex
  ): OperationCompiler[Eff] = fromServices(dsi.allServices)

  def fromServices(
    services: List[DynamicSchemaIndex.ServiceWrapper]
  ): OperationCompiler[Eff] = {
    val compilers: Map[QualifiedIdentifier, OperationCompiler[IorNel[CompilationError, *]]] =
      services.map { svc =>
        QualifiedIdentifier
          .forService(svc.service) -> OperationCompiler.fromService[svc.Alg, svc.Op](svc.service)
      }.toMap

    new MultiServiceCompiler(
      compilers,
      ServiceIndex.fromServices(services),
    )
  }

  def fromService[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
    service: Service[Alg, Op]
  ): OperationCompiler[IorNel[CompilationError, *]] = new ServiceCompiler(service)

  def seal[A](
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

}

final case class CompilationFailed(errors: NonEmptyList[CompilationError]) extends Throwable

object CompilationFailed {
  def one(e: CompilationError): CompilationFailed = CompilationFailed(NonEmptyList.one(e))

  val wrapK: IorNel[CompilationError, *] ~> IorThrow =
    new (IorNel[CompilationError, *] ~> IorThrow) {

      def apply[A](
        fa: Ior[NonEmptyList[CompilationError], A]
      ): IorThrow[A] = OperationCompiler.seal(fa).leftMap(CompilationFailed(_))

    }

}

private class ServiceCompiler[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
  service: Service[Alg, Op]
) extends OperationCompiler[IorNel[CompilationError, *]] {

  private def compileEndpoint[In, Err, Out](
    e: Endpoint[Op, In, Err, Out, _, _]
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
            type _Op[_I, _E, _O, _SE, _SO] = Op[_I, _E, _O, _SE, _SO]
            type E = Err
            type O = Out

            val op: Op[_, Err, Out, _, _] = e.wrap(compiled)
            val writeOutput: NodeEncoder[Out] = outputEncoder
            val writeError: Option[NodeEncoder[Err]] = errorEncoder
            val catchError: Throwable => Option[Err] = err => e.errorable.flatMap(_.liftError(err))
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

  private def deprecationWarnings(q: Query[WithSource]): IorNel[CompilationError, Unit] =
    (/* q.useClause.value */ ??? : Option[UseClause[WithSource]]) match {
      // todo: move to file compiler, support more clauses
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
    endpoint: Endpoint[Op, _, _, _, _, _],
  ) =
    endpoint
      .hints
      .get(api.Deprecated)
      .map { info =>
        CompilationError.deprecation(info, q.operationName.range)
      }
      .toBothLeft(())
      .toIorNel

  def compile(q: Query[WithSource]): IorNel[CompilationError, CompiledInput] = endpoints
    .get(q.operationName.value.operationName.value.text)
    .toRightIor(NonEmptyList.one(operationNotFound(q)))
    .flatTap { case (e, _) => deprecatedOperationCheck(q, e) }
    .flatMap(_._2.apply(q.input))

}

private class MultiServiceCompiler[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
  compilers: Map[QualifiedIdentifier, OperationCompiler[IorNel[CompilationError, *]]],
  serviceIndex: ServiceIndex,
) extends OperationCompiler[OperationCompiler.Eff] {

  private def getService(
    ctx: OperationCompiler.Context,
    q: Query[WithSource],
  ): EitherNel[CompilationError, OperationCompiler[IorNel[CompilationError, *]]] =
    MultiServiceResolver
      .resolveService(
        q.operationName.value.mapK(WithSource.unwrap),
        serviceIndex,
        useClauses = ctx.prelude.mapK(WithSource.unwrap).useClauses,
      )
      .leftMap(_.map(ResolutionFailure.toCompilationError(_, q)))
      .map(compilers(_))

  def compile(
    q: Query[WithSource]
  ): OperationCompiler.Eff[CompiledInput] = OperationCompiler
    .Eff
    .getContext
    .flatMapF(getService(_, q).pipe(Ior.fromEither(_)))
    .flatMapF(_.compile(q))

}
