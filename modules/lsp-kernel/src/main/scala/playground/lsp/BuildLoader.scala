package playground.lsp

import cats.effect.kernel.Sync
import cats.syntax.all.*
import fs2.io.file.Files
import fs2.io.file.Path
import playground.PlaygroundConfig
import playground.Uri
import playground.language.TextDocumentProvider
import software.amazon.smithy.model.Model

trait BuildLoader[F[_]] {

  def load(
    workspaceFolders: List[Uri]
  ): F[BuildLoader.Loaded]

  def buildModel(
    info: BuildLoader.Loaded
  ): F[Model]

}

object BuildLoader {

  def apply[F[_]](
    implicit F: BuildLoader[F]
  ): BuildLoader[F] = F

  case class Loaded(
    config: PlaygroundConfig,
    configFilePath: Path,
  )

  object Loaded {
    // Path is irrelevant when no imports are provided.
    val default: Loaded = Loaded(PlaygroundConfig.empty, Path("/"))
  }

  def instance[F[_]: TextDocumentProvider: Sync: Files]: BuildLoader[F] =
    new BuildLoader[F] {

      def load(
        workspaceFolders: List[Uri]
      ): F[BuildLoader.Loaded] = {
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
            PlaygroundConfig
              .decode(fileContents.getBytes())
              .liftTo[F]
              .map(BuildLoader.Loaded.apply(_, filePath))
          }
      }
        .adaptErr(new Exception("Failed to load build configuration", _))

      def buildModel(
        loaded: BuildLoader.Loaded
      ): F[Model] = {
        // This has to be lazy, because for the default, "no imports" config, the file path points to the filesystem root.
        lazy val workspaceBase = loaded
          .configFilePath
          .parent
          .getOrElse(sys.error("impossible - no parent for " + loaded.configFilePath))

        // "raw" means these can be directories etc., just like in the config file.
        val rawImportPaths =
          (
            loaded.config.imports ++
              loaded.config.sources
          ).map(workspaceBase.resolve).toSet

        for {
          specs <- filterImports(rawImportPaths)
          model <- loadModel(specs, loaded.config)
        } yield model
      }
        .adaptErr(new Exception("Failed to load model", _))

      private def loadModel(
        specs: Set[Path],
        config: PlaygroundConfig,
      ) = Sync[F]
        .interruptibleMany {
          ModelLoader.load(
            specs = specs.map(_.toNioPath.toFile),
            jars = ModelLoader.resolveModelDependencies(config),
          )
        }

      private def filterImports(
        specs: Set[Path]
      ): F[Set[Path]] = fs2
        .Stream
        .emits(specs.toSeq)
        .flatMap(Files[F].walk(_))
        .evalFilterNot(Files[F].isDirectory)
        .compile
        .to(Set)

    }

}
