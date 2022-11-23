package playground.lsp

import cats.effect.kernel.Async
import cats.effect.kernel.Sync
import cats.implicits._
import coursier._
import coursier.cache.FileCache
import coursier.parse.DependencyParser
import coursier.parse.RepositoryParser
import coursier.util.Task
import playground.BuildConfig
import playground.plugins.PlaygroundPlugin

import java.net.URLClassLoader
import scala.concurrent.duration._

trait PluginResolver[F[_]] {

  def resolve(
    artifacts: List[String],
    repositories: List[String],
  ): F[List[PlaygroundPlugin]]

  def resolveFromConfig(config: BuildConfig): F[List[PlaygroundPlugin]] = resolve(
    config
      .plugins
      .flatMap(_.smithyPlayground)
      .foldMap(_.extensions),
    config.mavenRepositories ++ config.maven.foldMap(_.repositories).map(_.url),
  )

}

object PluginResolver {

  def apply[F[_]](implicit F: PluginResolver[F]): PluginResolver[F] = F

  def instance[F[_]: Async]: PluginResolver[F] =
    new PluginResolver[F] {

      def resolve(
        artifacts: List[String],
        repositories: List[String],
      ): F[List[PlaygroundPlugin]] = {
        val depsF = DependencyParser
          .dependencies(artifacts, defaultScalaVersion = "2.13")
          .either
          .leftMap(errors =>
            new Throwable("Failed to parse dependencies: " + errors.mkString(", "))
          )
          .liftTo[F]

        val reposF = RepositoryParser
          .repositories(repositories)
          .either
          .leftMap(errors =>
            new Throwable("Failed to parse repositories: " + errors.mkString(", "))
          )
          .liftTo[F]

        (depsF, reposF)
          .mapN { (deps, repos) =>
            Fetch(FileCache[Task]().withTtl(1.hour))
              .addDependencies(deps: _*)
              .addRepositories(repos: _*)
          }
          .flatMap { fetch =>
            Async[F].fromFuture(Sync[F].delay(fetch.future()))
          }
          .flatMap { files =>
            Sync[F].delay {
              val classLoader =
                new URLClassLoader(
                  files.map(_.toURI().toURL()).toArray,
                  getClass().getClassLoader(),
                )

              PlaygroundPlugin.getAllPlugins(classLoader)
            }
          }

      }

    }

}
