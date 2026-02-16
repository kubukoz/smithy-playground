package playground.lsp

import coursierapi.*
import playground.PlaygroundConfig
import playground.lsp.buildinfo.BuildInfo
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.loader.ModelAssembler
import software.amazon.smithy.model.loader.ModelDiscovery
import software.amazon.smithy.model.loader.ModelManifestException

import java.io.File
import java.net.URI
import java.net.URL
import java.net.URLClassLoader
import java.nio.file.FileSystems
import java.nio.file.Files
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.chaining.*

// NOTE: methods in this object are mostly side effecting and blocking.
object ModelLoader {

  def makeClassLoaderForPlugins(
    buildConfig: PlaygroundConfig
  ): URLClassLoader = makeClassLoaderForJars(
    resolveDependencies(
      dependencies = buildConfig.extensions,
      repositories = buildConfig.repositories,
    )
  )

  private def makeClassLoaderForJars(
    jars: List[File]
  ): URLClassLoader =
    new URLClassLoader(
      jars.map(_.toURI().toURL()).toArray,
      this.getClass().getClassLoader(),
    )

  def load(
    specs: Set[File],
    jars: List[File],
  ): Model = Model
    .assembler()
    .putProperty(ModelAssembler.DISABLE_JAR_CACHE, true)
    .pipe(addJarModels(jars))
    .pipe(addPlaygroundModels(this.getClass().getClassLoader()))
    .pipe(addFileImports(specs))
    .assemble()
    .unwrap()

  // Credits: myself, in smithy4s 0.17.5
  private def addJarModels(
    jars: List[File]
  ): ModelAssembler => ModelAssembler = { m =>
    jars
      .flatMap(loadModelsFromJar)
      .foreach(
        m.addImport(_)
      )

    m
  }

  private def loadModelsFromJar(
    file: File
  ): List[URL] =
    Using.resource(
      // Note: On JDK13+, the second parameter is redundant.
      FileSystems.newFileSystem(file.toPath(), null: ClassLoader)
    ) { jarFS =>
      val manifestPath = jarFS.getPath("META-INF", "smithy", "manifest")

      // model discovery would throw if we tried to pass a non-existent path
      if (!Files.exists(manifestPath))
        Nil
      else {
        try ModelDiscovery.findModels(manifestPath.toUri().toURL()).asScala.toList
        catch {
          case e: ModelManifestException =>
            System
              .err
              .println(
                s"Unexpected exception while loading model from $file, skipping: $e"
              )

            Nil
        }
      }
    }

  private def addFileImports(
    imports: Iterable[File]
  ): ModelAssembler => ModelAssembler = { assembler =>
    imports.foreach(f => assembler.addImport(f.toPath()))
    assembler
  }

  private def addPlaygroundModels(
    classLoader: ClassLoader
  ): ModelAssembler => ModelAssembler = { assembler =>
    List(
      "META-INF/smithy/std.smithy"
    ).map(classLoader.getResource).foreach(assembler.addImport)

    assembler
  }

  def resolveModelDependencies(
    config: PlaygroundConfig
  ): List[File] = resolveDependencies(
    dependencies = config.dependencies,
    repositories = config.repositories,
  )

  def resolveDependencies(
    dependencies: List[String],
    repositories: List[String],
  ): List[File] = {
    val creds = coursierCredentialsByHost()

    val repos = repositories.map(MavenRepository.of).map(addCredsToRepo(_, creds))

    val deps = dependencies
      .map(Dependency.parse(_, ScalaVersion.of(BuildInfo.scalaBinaryVersion)))

    Fetch
      .create
      .addRepositories(repos*)
      .addDependencies(deps*)
      .fetch()
      .asScala
      .toList
  }

  // todo: add support for file credentials and other formats.
  // coursier-interface currently offers no way to access the standard credentials,
  // and I only needed support fro env vars, so here it is.
  private def coursierCredentialsByHost(): Map[String, Credentials] =
    sys
      .env
      .get("COURSIER_CREDENTIALS")
      .flatMap {
        case s"$host($_) $u:$p" => Some(host -> coursierapi.Credentials.of(u, p))
        // untested
        case s"$host $u:$p" => Some(host -> coursierapi.Credentials.of(u, p))
        case _              => None
      }
      .toMap

  private def addCredsToRepo(repo: MavenRepository, credsByHost: Map[String, Credentials])
    : MavenRepository = {
    val host = URI.create(repo.getBase()).getHost()
    credsByHost.get(host).foldLeft(repo)(_.withCredentials(_))
  }

}
