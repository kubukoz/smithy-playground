package playground.lsp

import cats.effect.kernel.Sync
import cats.implicits._
import playground.PlaygroundConfig
import playground.plugins.PlaygroundPlugin

trait PluginResolver[F[_]] {

  def resolve(
    config: PlaygroundConfig
  ): F[List[PlaygroundPlugin]]

}

object PluginResolver {

  def apply[F[_]](
    implicit F: PluginResolver[F]
  ): PluginResolver[F] = F

  def instance[F[_]: Sync]: PluginResolver[F] =
    new PluginResolver[F] {

      def resolve(
        config: PlaygroundConfig
      ): F[List[PlaygroundPlugin]] = Sync[F]
        .interruptibleMany(ModelLoader.makeClassLoaderForPluginsUnsafe(config))
        .map(PlaygroundPlugin.getAllPlugins(_))

    }

}
