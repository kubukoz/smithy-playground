package playground

import cats.data.EitherNel
import cats.data.Ior
import cats.data.IorNel
import cats.data.Kleisli
import cats.data.NonEmptyList
import cats.implicits._
import cats.~>
import playground._
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

  def compile(
    q: Query[WithSource]
  ): F[CompiledInput]

  def mapK[G[_]](
    fk: F ~> G
  ): OperationCompiler[G] =
    new OperationCompiler[G] {

      def compile(
        q: Query[WithSource]
      ): G[CompiledInput] = fk(self.compile(q))

    }

}

object OperationCompiler {

  final case class Context(
    prelude: Prelude[WithSource]
  )

  type EffF[F[_], A] = Kleisli[F, Context, A]
  type Eff[A] = EffF[IorNel[CompilationError, *], A]

  object Eff {

    val getContext: Eff[Context] = Kleisli.ask

    def perform(
      prelude: Prelude[WithSource]
    ): Eff ~> IorThrow = Kleisli
      .liftFunctionK(CompilationFailed.wrapK)
      .andThen(Kleisli.applyK(Context(prelude)))

  }

  def fromSchemaIndex(
    dsi: DynamicSchemaIndex
  ): OperationCompiler[Eff] = fromServices(dsi.allServices.toList)

  def fromServices(
    services: List[DynamicSchemaIndex.ServiceWrapper]
  ): OperationCompiler[Eff] = {
    val compilers: Map[QualifiedIdentifier, OperationCompiler[IorNel[CompilationError, *]]] =
      services.map { svc =>
        QualifiedIdentifier
          .forService(svc.service) -> OperationCompiler.fromService(svc.service)
      }.toMap

    new MultiServiceCompiler(
      compilers,
      ServiceIndex.fromServices(services),
    )
  }

  def fromService[Alg[_[_, _, _, _, _]]](
    service: Service[Alg]
  ): OperationCompiler[IorNel[CompilationError, *]] = new ServiceCompiler(service)

}

final case class CompilationFailed(
  errors: NonEmptyList[CompilationError]
) extends Throwable

object CompilationFailed {

  def one(
    e: CompilationError
  ): CompilationFailed = CompilationFailed(NonEmptyList.one(e))

  // this is a bit overused
  // https://github.com/kubukoz/smithy-playground/issues/157
  val wrapK: IorNel[CompilationError, *] ~> IorThrow =
    new (IorNel[CompilationError, *] ~> IorThrow) {

      def apply[A](
        fa: Ior[NonEmptyList[CompilationError], A]
      ): IorThrow[A] = seal(fa).leftMap(CompilationFailed(_))

      // https://github.com/kubukoz/smithy-playground/issues/157
      private def seal[A](
        result: IorNel[CompilationError, A]
      ): IorNel[CompilationError, A] = result.fold(
        Ior.left(_),
        Ior.right(_),
        (
          e,
          a,
        ) =>
          if (e.exists(_.isError))
            Ior.left(e)
          else
            Ior.both(e, a),
      )

    }

}

private class ServiceCompiler[Alg[_[_, _, _, _, _]]](
  service: Service[Alg]
) extends OperationCompiler[IorNel[CompilationError, *]] {

  private def compileEndpoint[In, Err, Out](
    e: Endpoint[service.Operation, In, Err, Out, SE, _]
  ): QueryCompiler[CompiledInput] = {
    val inputCompiler = e.input.compile(QueryCompilerVisitor.full)
    val outputEncoder = NodeEncoder.derive(e.output)
    val errorEncoder = e.error.map(e => NodeEncoder.derive(e.schema))

    ast =>
      inputCompiler
        .compile(ast)
        .map { compiled =>
          new CompiledInput {
            type _Op[_I, _E, _O, _SE, _SO] = service.Operation[_I, _E, _O, _SE, _SO]
            type E = Err
            type O = Out

            val op: _Op[_I, E, O, SE, SO] = e.wrap(compiled)
            val writeOutput: NodeEncoder[Out] = outputEncoder
            val writeError: Option[NodeEncoder[Err]] = errorEncoder
            val catchError: Throwable => Option[Err] = e.Error.unapply(_).map(_._2)
          }
        }
  }

  // https://github.com/kubukoz/smithy-playground/issues/154
  // map of endpoint names to (endpoint, input compiler)
  private val endpoints = service
    .endpoints
    .toList
    .groupByNel(_.name)
    .map(_.map(_.head).map(e => (e, compileEndpoint(e))))

  // Checks the explicit service reference (if any).
  // Note that the reference should be valid thanks to MultiServiceResolver's checks.
  private def deprecatedServiceCheck(
    q: Query[WithSource]
  ): IorNel[CompilationError, Unit] =
    q.operationName
      .value
      .identifier
      .flatMap { ref =>
        service
          .hints
          .get(api.Deprecated)
          .map(DeprecatedInfo.fromHint)
          .map(CompilationError.deprecation(_, ref.range))
      }
      .toBothLeft(())
      .toIorNel

  private def deprecatedOperationCheck(
    q: Query[WithSource],
    endpoint: Endpoint[service.Operation, _, _, _, _, _],
  ): IorNel[CompilationError, Unit] =
    endpoint
      .hints
      .get(api.Deprecated)
      .map { info =>
        CompilationError.deprecation(
          DeprecatedInfo.fromHint(info),
          q.operationName.value.operationName.range,
        )
      }
      .toBothLeft(())
      .toIorNel

  def compile(
    q: Query[WithSource]
  ): IorNel[CompilationError, CompiledInput] = {
    val (endpoint, inputCompiler) = endpoints
      .get(q.operationName.value.operationName.value.text)
      // https://github.com/kubukoz/smithy-playground/issues/154
      .getOrElse(
        sys.error(
          "Impossible! OperationCompiler running a query that doesn't belong to its operation."
        )
      )

    deprecatedServiceCheck(q) &>
      deprecatedOperationCheck(q, endpoint) &>
      inputCompiler.compile(q.input).leftMap(_.toNonEmptyList)
  }

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
        q.operationName.value,
        serviceIndex,
        useClauses = ctx.prelude.useClauses.map(_.value),
      )
      .map(compilers(_))

  def compile(
    q: Query[WithSource]
  ): OperationCompiler.Eff[CompiledInput] = OperationCompiler
    .Eff
    .getContext
    .flatMapF(getService(_, q).pipe(Ior.fromEither(_)))
    .flatMapF(_.compile(q))

}
