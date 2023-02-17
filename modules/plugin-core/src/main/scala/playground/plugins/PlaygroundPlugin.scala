package playground.plugins

import cats.effect.Concurrent
import org.http4s.client.Client
import smithy4s.Service
import smithy4s.UnsupportedProtocolError
import smithy4s.http4s.SimpleProtocolBuilder
import smithy4s.kinds._

import java.util.ServiceLoader
import scala.jdk.CollectionConverters._

trait PlaygroundPlugin {
  def simpleBuilders: List[SimpleHttpBuilder]
}

object PlaygroundPlugin {

  def getAllPlugins(loader: ClassLoader): List[PlaygroundPlugin] =
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
  ): Either[UnsupportedProtocolError, FunctorAlgebra[Alg, F]]

}

object SimpleHttpBuilder {

  def fromSimpleProtocolBuilder(builder: SimpleProtocolBuilder[_]): SimpleHttpBuilder =
    new SimpleHttpBuilder {

      def client[Alg[_[_, _, _, _, _]], F[_]: Concurrent](
        service: Service[Alg],
        backend: Client[F],
      ): Either[UnsupportedProtocolError, FunctorAlgebra[Alg, F]] =
        builder(service).client(backend).use

    }

}
