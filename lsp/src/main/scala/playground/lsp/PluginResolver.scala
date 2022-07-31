package playground.lsp

import coursier._
import cats.effect.kernel.Async
import cats.effect.kernel.Sync
import cats.implicits._
import coursier.parse.DependencyParser
import coursier.parse.RepositoryParser
import java.net.URLClassLoader
import java.util.ServiceLoader
import scala.jdk.CollectionConverters._
import playground.plugins.PlaygroundPlugin
import playground.BuildConfig

object PluginResolver {

  def resolveFromConfig[F[_]: Async](config: BuildConfig): F[List[PlaygroundPlugin]] = resolve[F](
    config
      .plugins
      .flatMap(_.smithyPlayground)
      .flatMap(_.extensions)
      .combineAll,
    config.mavenRepositories.combineAll,
  )

  def resolve[F[_]: Async](
    artifacts: List[String],
    repositories: List[String],
  ): F[List[PlaygroundPlugin]] = {
    val depsF = DependencyParser
      .dependencies(artifacts, defaultScalaVersion = "2.13")
      .either
      .leftMap(errors => new Throwable("Failed to parse dependencies: " + errors.mkString(", ")))
      .liftTo[F]

    val reposF = RepositoryParser
      .repositories(repositories)
      .either
      .leftMap(errors => new Throwable("Failed to parse repositories: " + errors.mkString(", ")))
      .liftTo[F]

    (depsF, reposF)
      .mapN { (deps, repos) =>
        Fetch()
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

          ServiceLoader
            .load(
              classOf[PlaygroundPlugin],
              classLoader,
            )
            .asScala
            .toList
        }
      }

  }

}
