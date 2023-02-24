package playground.lsp

import cats.effect.kernel.Sync
import cats.implicits._
import playground.BuildConfig
import playground.plugins.PlaygroundPlugin

trait PluginResolver[F[_]] {

  def resolve(
    artifacts: List[String],
    repositories: List[String],
  ): F[List[PlaygroundPlugin]]

  def resolveFromConfig(
    config: BuildConfig
  ): F[List[PlaygroundPlugin]] = resolve(
    config.mavenDependencies ++ config.maven.foldMap(_.dependencies) ++
      config
        .smithyPlayground
        .foldMap(_.extensions),
    config.mavenRepositories ++ config.maven.foldMap(_.repositories).map(_.url),
  )

}

object PluginResolver {

  def apply[F[_]](
    implicit F: PluginResolver[F]
  ): PluginResolver[F] = F

  def instance[F[_]: Sync]: PluginResolver[F] =
    new PluginResolver[F] {

      def resolve(
        artifacts: List[String],
        repositories: List[String],
      ): F[List[PlaygroundPlugin]] = Sync[F]
        .interruptibleMany(
          ModelLoader
            .loadUnsafe(
              specs = Set.empty,
              dependencies = artifacts,
              repositories = repositories,
            )
            ._1
        )
        .map(PlaygroundPlugin.getAllPlugins(_))

    }

}
