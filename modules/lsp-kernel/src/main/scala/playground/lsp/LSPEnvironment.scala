package playground.lsp

import cats.effect.std
import org.http4s.client.Client
import playground.plugins.Environment

// Provides the Environment instance for LSP type situations.
object LSPEnvironment {

  def instance[F[_]: std.Console: LanguageClient](
    using httpClient: Client[F]
  ): Environment[F] =
    new {
      def getK(k: Environment.Key): Option[k.Value[F]] = {
        val yolo: Option[Any] =
          k match {
            case Environment.httpClient => Some(httpClient)
            case Environment.baseUri =>
              Some(LanguageClient[F].configuration(ConfigurationValue.baseUri))
            case Environment.console => Some(std.Console[F])
            case _                   => None
          }
        yolo.map(_.asInstanceOf[k.Value[F]])
      }

    }

}
