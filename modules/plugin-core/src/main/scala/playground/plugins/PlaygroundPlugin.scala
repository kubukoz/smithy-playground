package playground.plugins

import smithy4s.http4s.SimpleProtocolBuilder
import smithy4s.Service
import org.http4s.client.Client
import smithy4s.UnsupportedProtocolError
import smithy4s.Monadic
import cats.effect.Concurrent
import java.util.ServiceLoader
import scala.jdk.CollectionConverters._
import scala.annotation.nowarn

trait PlaygroundPlugin {
  @deprecated("Implement simpleBuilders instead", "0.5.3")
  def http4sBuilders: List[SimpleProtocolBuilder[_]] = Nil

  @nowarn("cat=deprecation")
  def simpleBuilders: List[SimpleHttpBuilder] = http4sBuilders.map(
    SimpleHttpBuilder.fromSimpleProtocolBuilder
  )

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

  def client[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _], F[_]: Concurrent](
    service: Service[Alg, Op],
    backend: Client[F],
  ): Either[UnsupportedProtocolError, Monadic[Alg, F]]

}

object SimpleHttpBuilder {

  def fromSimpleProtocolBuilder(builder: SimpleProtocolBuilder[_]): SimpleHttpBuilder =
    new SimpleHttpBuilder {

      def client[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _], F[_]: Concurrent](
        service: Service[Alg, Op],
        backend: Client[F],
      ): Either[UnsupportedProtocolError, Monadic[Alg, F]] = builder(service).client(backend).use

    }

}
