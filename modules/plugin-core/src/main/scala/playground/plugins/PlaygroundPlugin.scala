package playground.plugins

import cats.data.IorNel
import cats.effect.Concurrent
import cats.effect.kernel.Async
import cats.effect.kernel.MonadCancelThrow
import cats.effect.std.Console
import cats.effect.syntax.all.*
import cats.syntax.all.*
import org.http4s.Uri
import org.http4s.client.Client
import smithy.api.ProtocolDefinition
import smithy4s.Service
import smithy4s.ShapeId
import smithy4s.UnsupportedProtocolError
import smithy4s.http4s.SimpleProtocolBuilder
import smithy4s.schema.Schema

import java.util.ServiceLoader
import scala.annotation.nowarn
import scala.jdk.CollectionConverters.*

trait PlaygroundPlugin {

  @deprecated(
    "Implement interpreters instead (e.g. Interpreter.fromSimpleBuilder). This method will be removed in the future"
  )
  def simpleBuilders: List[SimpleHttpBuilder] = Nil

  @nowarn("cat=deprecation")
  def interpreters[F[_]: Environment: Async]: List[Interpreter[F]] = simpleBuilders.map(
    Interpreter.fromSimpleBuilder(_)
  )

}

object PlaygroundPlugin {

  def getAllPlugins(
    loader: ClassLoader
  ): List[PlaygroundPlugin] =
    ServiceLoader
      .load(
        classOf[PlaygroundPlugin],
        loader,
      )
      .asScala
      .toList

}

/** A more flexible interface for SimpleProtocolBuilder-like things.
  */
trait SimpleHttpBuilder {

  def client[Alg[_[_, _, _, _, _]], F[_]: Concurrent](
    service: Service[Alg],
    backend: Client[F],
  ): Either[UnsupportedProtocolError, service.Impl[F]]

}

object SimpleHttpBuilder {

  def fromSimpleProtocolBuilder(
    builder: SimpleProtocolBuilder[?]
  ): SimpleHttpBuilder =
    new SimpleHttpBuilder {

      def client[Alg[_[_, _, _, _, _]], F[_]: Concurrent](
        service: Service[Alg],
        backend: Client[F],
      ): Either[UnsupportedProtocolError, service.Impl[F]] = builder(service).client(backend).make

    }

}

trait Environment[F[_]] {

  def getK(k: Environment.Key): Option[k.Value[F]]

  def requireK(k: Environment.Key): k.Value[F] = getK(k).getOrElse(
    sys.error(s"Required key $k not found in Environment")
  )

}

object Environment {

  def apply[F[_]](
    using F: Environment[F]
  ): Environment[F] = F

  trait Key {
    type Value[F[_]]

    def require[F[_]](
      using Environment[F]
    ): Value[F] = Environment[F].requireK(this)

  }

  object Key {
    type Aux[Alg[_[_]]] = Key { type Value[F[_]] = Alg[F] }
  }

  // todo: replace with some general config source
  val baseUri: Key.Aux[[F[_]] =>> F[Uri]] = k[[F[_]] =>> F[Uri]]("baseUri")
  val httpClient: Key.Aux[Client] = k("httpClient")
  val console: Key.Aux[Console] = k("console")

  private def k[Alg[_[_]]](name: String): Key { type Value[F[_]] = Alg[F] } =
    new Key {
      type Value[F[_]] = Alg[F]
      override def toString(): String = s"Environment.Key($name)"
    }

}

trait Interpreter[F[_]] {

  def provide[Alg[_[_, _, _, _, _]]](
    service: Service[Alg],
    schemaIndex: ShapeId => Option[Schema[?]],
  ): IorNel[Interpreter.Issue, service.FunctorInterpreter[F]]

}

object Interpreter {

  def fromSimpleBuilder[F[_]: Environment: Async](sb: SimpleHttpBuilder): Interpreter[F] = {
    given Console[F] = Environment[F].requireK(Environment.console)

    http[F](
      Environment.baseUri.require,
      Environment.httpClient.require,
      sb,
    )
  }

  // todo: remove this from the Issue.InvalidProtocol structure, inject later, centralized
  def protocols[Alg[_[_, _, _, _, _]]](
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

  private def http[F[_]: cats.effect.std.Console: Concurrent](
    baseUri: F[Uri],
    client: Client[F],
    builder: SimpleHttpBuilder,
  ): Interpreter[F] =
    new {
      def provide[Alg[_[_, _, _, _, _]]](
        service: Service[Alg],
        schemaIndex: ShapeId => Option[Schema[?]],
      ): IorNel[Issue, service.FunctorInterpreter[F]] =
        builder
          .client(
            service,
            dynamicBaseUri[F](
              baseUri.flatTap { uri =>
                Console[F].println(s"Using base URI: $uri")
              }
            ).apply(client),
          )
          .leftMap(e => Issue.InvalidProtocol(e.protocolTag.id, protocols(service, schemaIndex)))
          .map(service.toPolyFunction(_))
          .toIor
          .toIorNel
    }

  // https://github.com/kubukoz/smithy-playground/issues/158
  private def dynamicBaseUri[F[_]: MonadCancelThrow](
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

  enum Issue {

    case InvalidProtocol(
      supported: ShapeId,
      found: List[ShapeId],
    )

    case Other(e: Throwable)

  }

}
