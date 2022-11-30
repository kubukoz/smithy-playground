package playground.lsp

// fork of smithy4s's ModelLoader

import cats.effect.Sync
import coursier._
import coursier.cache.FileCache
import coursier.parse.DependencyParser
import coursier.parse.RepositoryParser
import coursier.util.Task
import playground.lsp.buildinfo.BuildInfo
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.loader.ModelAssembler

import java.io.File
import java.net.URL
import java.net.URLClassLoader

object ModelLoader {

  def load[F[_]: Sync](
    specs: Set[File],
    dependencies: List[String],
    repositories: List[String],
  ): F[(ClassLoader, Model)] = Sync[F].interruptibleMany { // blocking just for convenience
    val currentClassLoader = this.getClass().getClassLoader()

    val modelAssembler: ModelAssembler = Model
      .assembler()
      .putProperty(ModelAssembler.DISABLE_JAR_CACHE, true)

    // add local imports
    specs.map(_.toPath()).foreach {
      modelAssembler.addImport
    }

    // fetch deps
    val modelClassLoader =
      new URLClassLoader(resolveDependencies(dependencies, repositories).toArray)

    // add deps to assembler
    modelAssembler.discoverModels(modelClassLoader)

    // add playground shapes - this depends on alloy, which should already be in scope
    addPlaygroundModels(currentClassLoader)(modelAssembler): Unit

    val model = modelAssembler.assemble().unwrap()

    (modelClassLoader, model)
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
  ): List[URL] = {
    val maybeRepos = RepositoryParser.repositories(repositories).either
    val maybeDeps =
      DependencyParser
        .dependencies(
          // quick hack
          "com.disneystreaming.alloy:alloy-core:0.1.2" :: dependencies,
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

    Fetch(FileCache[Task]())
      .addRepositories(repos: _*)
      .addDependencies(deps: _*)
      .run()
      .map(_.toURI.toURL)
      .toList
  }

}
