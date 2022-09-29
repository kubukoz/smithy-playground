package playground.lsp

import cats.effect.kernel.Sync
import cats.implicits._
import fs2.io.file.Path
import playground.BuildConfig
import playground.BuildConfigDecoder
import playground.ModelReader
import playground.TextDocumentProvider
import smithy4s.codegen.ModelLoader
import smithy4s.dynamic.DynamicSchemaIndex

trait BuildLoader[F[_]] {
  def load: F[BuildLoader.Loaded]

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

      def load: F[BuildLoader.Loaded] = {
        val configFiles = List(
          "build/smithy-dependencies.json",
          ".smithy.json",
          "smithy-build.json",
        )

        fs2
          .Stream
          .emit(Path("."))
          .flatMap { folder =>
            fs2
              .Stream
              .emits(configFiles)
              .map(folder.resolve(_).absolute)
          }
          .evalMap(filePath =>
            TextDocumentProvider[F]
              .getOpt(
                filePath
                  .toNioPath
                  .toUri()
                  .toString()
              )
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
              .tupleRight(filePath)
              .map(BuildLoader.Loaded.apply.tupled)
          }
      }

      def buildSchemaIndex(loaded: BuildLoader.Loaded): F[DynamicSchemaIndex] = Sync[F]
        .interruptibleMany {
          ModelLoader
            .load(
              specs =
                loaded
                  .config
                  .imports
                  .combineAll
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
              dependencies = loaded.config.mavenDependencies.combineAll,
              repositories = loaded.config.mavenRepositories.combineAll,
              transformers = Nil,
              discoverModels = true,
            )
            ._2
        }
        .flatMap(ModelReader.buildSchemaIndex[F])

    }

}
