package playground.lsp

// fork of smithy4s's ModelLoader

import coursier._
import coursier.parse.DependencyParser
import coursier.parse.RepositoryParser
import playground.lsp.buildinfo.BuildInfo
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.loader.ModelAssembler

import java.io.File
import java.net.URL
import java.net.URLClassLoader
import scala.util.chaining._
import cats.effect.Sync

object ModelLoader {

  def load[F[_]: Sync](
    specs: Set[File],
    dependencies: List[String],
    repositories: List[String],
  ): F[(ClassLoader, Model)] = Sync[F].interruptibleMany { // blocking just for convenience
    val currentClassLoader = this.getClass().getClassLoader()

    val modelAssembler: ModelAssembler = Model
      .assembler()
      .pipe(addPlaygroundModels(currentClassLoader))
      .putProperty(ModelAssembler.DISABLE_JAR_CACHE, true)

    // add imports/specs

    specs.map(_.toPath()).foreach {
      modelAssembler.addImport
    }

    // fetch and add deps

    val modelClassLoader =
      resolveDependencies(dependencies, repositories) match {
        case Some(deps) =>
          val upstreamClassLoader = new URLClassLoader(deps)

          modelAssembler.discoverModels(upstreamClassLoader)

          upstreamClassLoader

        case None => currentClassLoader
      }

    val model = modelAssembler.assemble().unwrap()

    (modelClassLoader, model)
  }

  private def addPlaygroundModels(
    classLoader: ClassLoader
  ): ModelAssembler => ModelAssembler =
    assembler => {
      List(
        "META-INF/smithy/std.smithy",
        // for UUID
        "META-INF/smithy/smithy4s.smithy",
      ).map(classLoader.getResource).foreach(assembler.addImport)

      assembler
    }

  private def resolveDependencies(
    dependencies: List[String],
    repositories: List[String],
  ): Option[Array[URL]] = {
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
    val resolvedDeps: Seq[java.io.File] =
      if (deps.nonEmpty) {
        val fetch = Fetch().addRepositories(repos: _*).addDependencies(deps: _*)
        fetch.run()
      } else {
        Seq.empty
      }
    val allDeps = resolvedDeps
    if (allDeps.nonEmpty) {
      Some(allDeps.map(_.toURI().toURL()).toArray)
    } else {
      None
    }
  }

}
