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
  type Params
  def prepare: F[ServerReload.PrepareResult[Params]]
  def perform(params: Params): F[ServerReload.WorkspaceStats]
}

object ServerReload {
  def apply[F[_]](implicit F: ServerReload[F]): F.type = F

  case class PrepareResult[A](params: A, isChanged: Boolean)
  case class WorkspaceStats(importCount: Int, dependencyCount: Int, pluginCount: Int)

  object WorkspaceStats {

    def fromBuildConfig(bc: BuildConfig): WorkspaceStats = WorkspaceStats(
      importCount = bc.imports.combineAll.size,
      dependencyCount = bc.mavenDependencies.combineAll.size,
      pluginCount = bc.plugins.flatMap(_.smithyPlayground).flatMap(_.extensions).combineAll.size,
    )

  }

  def instance[
    F[_]: TextDocumentManager: BuildLoader: LanguageClient: PluginResolver: Supervisor: Async: std.Console
  ](
    serverRef: Ref[F, (LanguageServer[F], Option[BuildConfig])],
    client: Client[F],
    awsEnv: Resource[F, AwsEnvironment[F]],
  ): F[ServerReload[F]] = {
    val instance =
      new ServerReload[F] {
        implicit val sr: ServerReload[F] = this
        type Params = (BuildConfig, Path)

        def prepare: F[PrepareResult[Params]] = serverRef.get.map(_._2).flatMap {
          previousBuildConfig =>
            BuildLoader[F].load.map { case params @ (bc, _) =>
              PrepareResult(params, previousBuildConfig.contains(bc))
            }
        }

        private implicit val uriJsonDecoder: Decoder[Uri] = Decoder[String].emap(
          Uri.fromString(_).leftMap(_.message)
        )

        def perform(params: Params): F[WorkspaceStats] = BuildLoader[F]
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
          .as(WorkspaceStats.fromBuildConfig(params._1))

      }

    // Initial load
    instance.prepare.map(_.params).flatMap(instance.perform).as(instance)
  }

}
