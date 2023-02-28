package playground.lsp

import cats.effect.kernel.Sync
import cats.implicits._
import playground.BuildConfig
import playground.plugins.PlaygroundPlugin

trait PluginResolver[F[_]] {

  def resolve(
    config: BuildConfig
  ): F[List[PlaygroundPlugin]]

}

object PluginResolver {

  def apply[F[_]](
    implicit F: PluginResolver[F]
  ): PluginResolver[F] = F

  def instance[F[_]: Sync]: PluginResolver[F] =
    new PluginResolver[F] {

      def resolve(
        config: BuildConfig
      ): F[List[PlaygroundPlugin]] = Sync[F]
        .interruptibleMany(ModelLoader.makeClassLoaderUnsafe(config))
        .map(PlaygroundPlugin.getAllPlugins(_))

    }

}
