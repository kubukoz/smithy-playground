package playground.lsp

import coursier._
import coursier.cache.FileCache
import coursier.parse.DependencyParser
import coursier.parse.RepositoryParser
import coursier.util.Task
import playground.PlaygroundConfig
import playground.lsp.buildinfo.BuildInfo
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.loader.ModelAssembler

import java.io.File
import java.net.URLClassLoader
import scala.concurrent.duration._
import scala.util.chaining._

object ModelLoader {

  def makeClassLoaderUnsafe(
    buildConfig: PlaygroundConfig
  ): URLClassLoader = makeClassLoaderForJars(
    resolveDependencies(
      dependencies = buildConfig.dependencies,
      repositories = buildConfig.repositories,
    ),
    isolated = true,
  )

  def makeClassLoaderForPluginsUnsafe(
    buildConfig: PlaygroundConfig
  ): URLClassLoader = makeClassLoaderForJars(
    resolveDependencies(
      dependencies = buildConfig.extensions,
      repositories = buildConfig.repositories,
    ),
    isolated = false,
  )

  private def makeClassLoaderForJars(
    jars: List[File],
    isolated: Boolean,
  ): URLClassLoader =
    new URLClassLoader(
      jars.map(_.toURI().toURL()).toArray,
      if (isolated)
        null
      else
        this.getClass().getClassLoader(),
    )

  def load(
    specs: Set[File],
    classLoader: URLClassLoader,
  ): Model = Model
    .assembler()
    .putProperty(ModelAssembler.DISABLE_JAR_CACHE, true)
    .discoverModels(classLoader)
    .pipe(addPlaygroundModels(this.getClass().getClassLoader()))
    .pipe(addFileImports(specs))
    .assemble()
    .unwrap()

  private def addFileImports(
    imports: Iterable[File]
  ): ModelAssembler => ModelAssembler =
    assembler => {
      imports.foreach(f => assembler.addImport(f.toPath()))
      assembler
    }

  private def addPlaygroundModels(
    classLoader: ClassLoader
  ): ModelAssembler => ModelAssembler =
    assembler => {
      List(
        "META-INF/smithy/std.smithy"
      ).map(classLoader.getResource).foreach(assembler.addImport)

      assembler
    }

  private def resolveDependencies(
    dependencies: List[String],
    repositories: List[String],
  ): List[File] = {
    val maybeRepos = RepositoryParser.repositories(repositories).either
    val maybeDeps =
      DependencyParser
        .dependencies(
          dependencies,
          defaultScalaVersion = BuildInfo.scalaBinaryVersion,
        )
        .either
    val repos =
      maybeRepos match {
        case Left(errorMessages) =>
          throw new IllegalArgumentException(
            s"Failed to parse repositories with error: $errorMessages"
          )
        case Right(r) => r
      }
    val deps =
      maybeDeps match {
        case Left(errorMessages) =>
          throw new IllegalArgumentException(
            s"Failed to parse dependencies with errors: $errorMessages"
          )
        case Right(d) => d
      }

    Fetch(FileCache[Task]().withTtl(1.hour))
      .addRepositories(repos: _*)
      .addDependencies(deps: _*)
      .run()
      .toList
  }

}
