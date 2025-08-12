package playground.lsp

import cats.effect.kernel.Sync
import cats.syntax.all.*
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
        .interruptibleMany(ModelLoader.makeClassLoaderForPlugins(config))
        .map(PlaygroundPlugin.getAllPlugins(_))
        .adaptErr(new Exception("Failed to resolve plugins", _))

    }

}
