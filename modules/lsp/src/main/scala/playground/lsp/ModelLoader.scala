package playground.lsp

// fork of smithy4s's ModelLoader

import coursier._
import coursier.cache.FileCache
import coursier.parse.DependencyParser
import coursier.parse.RepositoryParser
import coursier.util.Task
import playground.lsp.buildinfo.BuildInfo
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.loader.ModelAssembler
import software.amazon.smithy.model.loader.ModelDiscovery
import software.amazon.smithy.model.loader.ModelManifestException

import java.io.File
import java.net.URLClassLoader
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.chaining._

object ModelLoader {

  def loadUnsafe(
    specs: Set[File],
    dependencies: List[String],
    repositories: List[String],
  ): (
    ClassLoader,
    Model,
  ) = {

    val dependencyJars = resolveDependencies(dependencies, repositories)

    val modelAssembler: ModelAssembler = Model
      .assembler()
      .putProperty(ModelAssembler.DISABLE_JAR_CACHE, true)
      .pipe(addModelsFromJars(dependencyJars))
      .pipe(addPlaygroundModels(this.getClass().getClassLoader()))
      .pipe(addFileImports(specs))

    (
      new URLClassLoader(
        dependencyJars.map(_.toURI().toURL()).toArray,
        getClass().getClassLoader(),
      ),
      modelAssembler.assemble().unwrap(),
    )
  }

  private def addModelsFromJars(
    jarFiles: Iterable[File]
  ): ModelAssembler => ModelAssembler =
    assembler => {
      val modelsInJars = jarFiles.flatMap { file =>
        val manifestUrl = ModelDiscovery.createSmithyJarManifestUrl(file.getAbsolutePath())
        try ModelDiscovery.findModels(manifestUrl).asScala
        catch {
          case _: ModelManifestException => Nil
        }
      }

      modelsInJars.foreach(assembler.addImport)
      assembler
    }

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
