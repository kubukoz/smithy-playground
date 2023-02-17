package playground.lsp

import cats.effect.kernel.Sync
import cats.implicits._
import fs2.io.file.Path
import playground.BuildConfig
import playground.BuildConfigDecoder
import playground.ModelReader
import playground.language.TextDocumentProvider
import playground.language.Uri
import smithy4s.dynamic.DynamicSchemaIndex

trait BuildLoader[F[_]] {
  def load(workspaceFolders: List[Uri]): F[BuildLoader.Loaded]

  def buildSchemaIndex(info: BuildLoader.Loaded): F[DynamicSchemaIndex]

}

object BuildLoader {
  def apply[F[_]](implicit F: BuildLoader[F]): BuildLoader[F] = F

  case class Loaded(config: BuildConfig, configFilePath: Path)

  object Loaded {
    // Path is irrelevant when no imports are provided.
    val default: Loaded = Loaded(BuildConfig(), Path("/"))
  }

  def instance[F[_]: TextDocumentProvider: Sync]: BuildLoader[F] =
    new BuildLoader[F] {

      def load(workspaceFolders: List[Uri]): F[BuildLoader.Loaded] = {
        val configFiles = List(
          "build/smithy-dependencies.json",
          ".smithy.json",
          "smithy-build.json",
        )

        // For now, we only support a single workspace folder.
        fs2
          .Stream
          .emit(
            workspaceFolders.headOption.getOrElse(sys.error("no workspace folders found")).toPath
          )
          .flatMap { folder =>
            fs2
              .Stream
              .emits(configFiles)
              .map(folder.resolve(_).absolute)
          }
          .evalMap(filePath =>
            TextDocumentProvider[F]
              .getOpt(Uri.fromPath(filePath))
              .map(_.tupleRight(filePath))
          )
          .unNone
          .head
          .compile
          .last
          .flatMap {
            _.liftTo[F](
              new Throwable(
                "Couldn't find one of the following files: " + configFiles.mkString(", ")
              )
            )
          }
          .flatMap { case (fileContents, filePath) =>
            BuildConfigDecoder
              .decode(fileContents.getBytes())
              .liftTo[F]
              .map(BuildLoader.Loaded.apply(_, filePath))
          }
      }

      def buildSchemaIndex(loaded: BuildLoader.Loaded): F[DynamicSchemaIndex] = Sync[F]
        .interruptibleMany {
          ModelLoader
            .loadUnsafe(
              specs =
                loaded
                  .config
                  .imports
                  .map(
                    loaded
                      .configFilePath
                      .parent
                      .getOrElse(sys.error("impossible - no parent"))
                      .resolve(_)
                      .toNioPath
                      .toFile()
                  )
                  .toSet,
              dependencies =
                loaded.config.mavenDependencies ++ loaded.config.maven.foldMap(_.dependencies),
              repositories =
                loaded
                  .config
                  .mavenRepositories ++ loaded.config.maven.foldMap(_.repositories).map(_.url),
            )
        }
        .map(_._2)
        .flatMap(ModelReader.buildSchemaIndex[F])

    }

}
