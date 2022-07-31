package playground.lsp

import cats.effect.kernel.Async
import cats.effect.kernel.Ref
import cats.effect.kernel.Resource
import cats.effect.std
import cats.effect.std.Supervisor
import cats.implicits._
import fs2.io.file.Path
import io.circe.Decoder
import org.http4s.Uri
import org.http4s.client.Client
import playground.BuildConfig
import playground.Runner
import playground.TextDocumentManager
import smithy4s.aws.AwsEnvironment

trait ServerReload[F[_]] {
  type Params = (BuildConfig, Path)

  def prepare: F[Option[Params]]
  def perform(params: Params): F[Unit]
}

object ServerReload {
  def apply[F[_]](implicit F: ServerReload[F]): ServerReload[F] = F

  def instance[
    F[_]: TextDocumentManager: BuildLoader: LanguageClient: PluginResolver: Supervisor: Async: std.Console
  ](
    serverRef: Ref[F, (LanguageServer[F], Option[BuildConfig])],
    client: Client[F],
    awsEnv: Resource[F, AwsEnvironment[F]],
  ): ServerReload[F] =
    new ServerReload[F] {
      implicit val sr: ServerReload[F] = this
      type Token = Path

      def prepare: F[Option[Params]] = serverRef.get.map(_._2).flatMap { previousBuildConfig =>
        BuildLoader[F].load.map {
          case (bc, _) if previousBuildConfig.contains(bc) => none
          case pair                                        => pair.some
        }
      }

      private implicit val uriJsonDecoder: Decoder[Uri] = Decoder[String].emap(
        Uri.fromString(_).leftMap(_.message)
      )

      def perform(params: Params): F[Unit] = BuildLoader[F]
        .buildSchemaIndex
        .tupled(params)
        .flatMap { dsi =>
          PluginResolver[F]
            .resolveFromConfig(params._1)
            .map { plugins =>
              val runner = Runner
                .forSchemaIndex[F](
                  dsi,
                  client,
                  LanguageClient[F]
                    .configuration[Uri]("smithyql.http.baseUrl"),
                  awsEnv,
                  plugins = plugins,
                )

              LanguageServer.instance[F](dsi, runner)
            }
        }
        .tupleRight(params._1.some)
        .flatMap(serverRef.set)

    }

}
