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
import smithy4s.Service
import smithy4s.ShapeId
import smithy4s.UnsupportedProtocolError
import smithy4s.http4s.SimpleProtocolBuilder
import smithy4s.schema.Schema

import java.util.ServiceLoader
import scala.jdk.CollectionConverters.*

trait PlaygroundPlugin {
  def interpreters[F[_]: Environment: Async]: List[Interpreter[F]]
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
      ): Either[UnsupportedProtocolError, service.Impl[F]] =
        builder
          .apply(service)
          .client(backend)
          .make
    }

}

// Note: new methods can be added without notice. Assume this is always provided by Playground.
trait Environment[F[_]] {

  def getK[Value[_[_]]](k: Environment.Key[Value]): Option[Value[F]]

  def requireK[Value[_[_]]](k: Environment.Key[Value]): Value[F] = getK(k).getOrElse(
    throw new NoSuchElementException(s"Environment key not found: $k")
  )

}

object Environment {

  def apply[F[_]](
    using F: Environment[F]
  ): Environment[F] = F

  trait Key[Value[F[_]]] {

    def require[F[_]](
      using Environment[F]
    ): Value[F] = Environment[F].requireK(this)

  }

  // note: might be replaced with some general config source in the future
  val baseUri: Key[[F[_]] =>> F[Uri]] = k("baseUri")
  val httpClient: Key[Client] = k("httpClient")
  val console: Key[Console] = k("console")

  private def k[Alg[_[_]]](name: String): Key[Alg] =
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

  def fromSimpleBuilder[F[_]: Environment: Concurrent](sb: SimpleHttpBuilder): Interpreter[F] = {
    given Console[F] = Environment[F].requireK(Environment.console)

    http[F](
      Environment.baseUri.require,
      Environment.httpClient.require,
      sb,
    )
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
          .leftMap(e => Issue.InvalidProtocol(e.protocolTag.id))
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

    case InvalidProtocol(supported: ShapeId)

    case Other(e: Throwable)

  }

}
