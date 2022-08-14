package playground

import aws.protocols.AwsJson1_0
import aws.protocols.AwsJson1_1
import cats.Defer
import cats.Id
import cats.data.Ior
import cats.data.IorNel
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.effect.implicits._
import cats.effect.std
import cats.implicits._
import cats.~>
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.implicits._
import playground._
import playground.plugins.PlaygroundPlugin
import playground.smithyql.InputNode
import playground.smithyql.OperationName
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.Query
import playground.smithyql.WithSource
import playground.std.Stdlib
import playground.std.StdlibRuntime
import smithy.api
import smithy.api.ProtocolDefinition
import smithy4s.Endpoint
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

trait Compiler[F[_]] { self =>
  def compile(q: Query[WithSource]): F[CompiledInput]

  def mapK[G[_]](fk: F ~> G): Compiler[G] =
    new Compiler[G] {
      def compile(q: Query[WithSource]): G[CompiledInput] = fk(self.compile(q))
    }

}

object Compiler {

  def fromSchemaIndex(
    dsi: DynamicSchemaIndex
  ): Compiler[Ior[Throwable, *]] = {
    val services: Map[QualifiedIdentifier, Compiler[Ior[Throwable, *]]] =
      dsi
        .allServices
        .map { svc =>
          QualifiedIdentifier
            .forService(svc.service) -> Compiler.fromService[svc.Alg, svc.Op](svc.service)
        }
        .toMap

    new MultiServiceCompiler(services)
  }

  def fromService[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
    service: Service[Alg, Op]
  ): Compiler[Ior[Throwable, *]] = new ServiceCompiler(service)

}

final case class CompilationFailed(errors: NonEmptyList[CompilationError]) extends Throwable

object CompilationFailed {
  def one(e: CompilationError): CompilationFailed = CompilationFailed(NonEmptyList.one(e))
}

private class ServiceCompiler[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
  service: Service[Alg, Op]
) extends Compiler[Ior[Throwable, *]] {

  private def compileEndpoint[In, Err, Out](
    e: Endpoint[Op, In, Err, Out, _, _]
  ): WithSource[InputNode[WithSource]] => IorNel[CompilationError, CompiledInput] = {
    val inputCompiler = e.input.compile(QueryCompiler)
    val outputEncoder = NodeEncoder.derive(e.output)
    val errorEncoder = e.errorable.map(e => NodeEncoder.derive(e.error))

    ast =>
      inputCompiler
        .compile(ast)
        .leftMap(_.toNonEmptyList)
        .map { compiled =>
          new CompiledInput {
            type _Op[_I, _E, _O, _SE, _SO] = Op[_I, _E, _O, _SE, _SO]
            type I = In
            type E = Err
            type O = Out
            val input: I = compiled
            val serviceId: QualifiedIdentifier = QualifiedIdentifier.forService(service)

            def wrap(i: In): Op[In, Err, Out, _, _] = e.wrap(i)
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

private class MultiServiceCompiler[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
  services: Map[QualifiedIdentifier, Compiler[Ior[Throwable, *]]]
) extends Compiler[Ior[Throwable, *]] {

  private def getService(
    q: Query[WithSource]
  ): Either[Throwable, Compiler[Ior[Throwable, *]]] = MultiServiceResolver
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

trait Runner[F[_]] {
  def run(q: CompiledInput): F[InputNode[Id]]
}

object Runner {

  trait Resolver[F[_]] {
    def get(parsed: Query[WithSource]): IorNel[Issue, Runner[F]]
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
            Runner.forService[svc.Alg, svc.Op, F](
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
      def get(q: Query[WithSource]): IorNel[Issue, Runner[F]] = MultiServiceResolver
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
      ): IorNel[Issue, smithy4s.Interpreter[Op, F]] =
        Either
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
            _.leftMap(e => Issue.InvalidProtocol(e.protocolTag.id, serviceProtocols))
          }
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

          proxy
            .tryProxy(StdlibRuntime[F].random)
            .orElse(proxy.tryProxy(StdlibRuntime[F].clock))
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

      private def perform[I, E, O](
        interpreter: smithy4s.Interpreter[Op, F],
        q: CompiledInput.Aux[I, E, O, Op],
      ) = Defer[F].defer(interpreter(q.wrap(q.input))).map { response =>
        q.writeOutput.toNode(response)
      }

      val runners: NonEmptyList[IorNel[Issue, Runner[F]]] = NonEmptyList
        .of(
          simpleFromBuilder(SimpleRestJsonBuilder),
          awsInterpreter,
        )
        .concat(plugins.flatMap(_.http4sBuilders).map(simpleFromBuilder))
        .append(stdlibRunner)
        .map(
          _.map { interpreter =>
            new Runner[F] {
              def run(q: CompiledInput): F[InputNode[Id]] = perform[q.I, q.E, q.O](
                interpreter,
                // note: this is safe... for real
                q.asInstanceOf[CompiledInput.Aux[q.I, q.E, q.O, Op]],
              )
            }
          }
        )

      val getInternal: IorNel[Issue, Runner[F]] = runners.reduce(
        IorUtils.orElseCombine[NonEmptyList[Issue], Runner[F]]
      )

      def get(parsed: Query[WithSource]): IorNel[Issue, Runner[F]] = getInternal
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
