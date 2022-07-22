package playground

import cats.effect.kernel.Async
import cats.effect.kernel.Sync
import cats.implicits._
import playground.buildinfo.BuildInfo
import smithy4s.dynamic.DynamicSchemaIndex
import typings.node.BufferEncoding
import typings.node.nodeChildProcessMod
import typings.vscode.mod

import scalajs.js
import typings.node.childProcessMod.ExecSyncOptions
import cats.effect.std

object build {

  val configFiles = List("build/smithy-dependencies.json", ".smithy.json", "smithy-build.json")

  def buildFile[F[_]: Async: std.Console]: F[(BuildConfig, mod.Uri)] = fs2
    .Stream
    .exec(
      std.Console[F].println(s"Loading config from ${configFiles.mkString(", ")}...")
    )
    .append(fs2.Stream.emits(configFiles))
    .evalMap { template =>
      Async[F]
        .fromFuture {
          Sync[F].delay {
            mod
              .workspace
              .findFiles(template)
              .asInstanceOf[js.Thenable[js.Array[mod.Uri]]]
              .toFuture
          }
        }
        .map(_.toList)
    }
    .flatMap(fs2.Stream.emits(_))
    .evalMap { uri =>
      Async[F]
        .fromFuture {
          Sync[F].delay {
            mod
              .workspace
              .openTextDocument(uri)
              .asInstanceOf[js.Thenable[mod.TextDocument]]
              .toFuture
          }
        }
        .tupleRight(uri)
    }
    .head
    .compile
    .lastOrError
    .flatTap { _ =>
      std.Console[F].println("Parsing config...")
    }
    .flatMap { case (doc, uri) =>
      BuildConfigDecoder.decode(doc.getText().getBytes()).liftTo[F].tupleRight(uri)
    }

  def getServices[F[_]: std.Console: Async](
    cwd: String,
    buildFile: BuildConfig,
  ): F[DynamicSchemaIndex] =
    debug.timedF("getService") {

      std.Console[F].println("Dumping model...") *>
        runDump(buildFile, cwd)
          .flatMap { modelText =>
            std.Console[F].println("Parsing model...") *>
              debug.timedF("parse-model")(ModelReader.modelParser(modelText).liftTo[F])
          }
          .flatMap { decodedModel =>
            std.Console[F].println("Loading schemas...") *>
              debug
                .timedF("DSI.load") {
                  Sync[F].delay(ModelReader.buildSchemaIndex(decodedModel))
                }
          }
          .flatTap { services =>
            std
              .Console[F]
              .println(
                "Loaded services: " + services
                  .allServices
                  .map(_.service.id.show)
                  .mkString(", ") + "\n\n"
              )
          }
    }

  private def runDump[F[_]: std.Console: Async](buildFile: BuildConfig, cwd: String): F[String] = {
    val repos = buildFile
      .mavenRepositories
      .combineAll
      .toNel
      .foldMap(repos => "--repositories" :: repos.mkString_(",") :: Nil)
    val deps = buildFile
      .mavenDependencies
      .combineAll
      .toNel
      .foldMap(deps => "--dependencies" :: deps.mkString_(",") :: Nil)

    val args =
      "dump-model" ::
        buildFile.imports.combineAll :::
        repos :::
        deps

    debug
      .timedF("dump-model") {
        Sync[F].delay {
          nodeChildProcessMod.execSync(
            // todo: pass version from workspace config, default from sbt-buildinfo
            ("cs" :: "launch" :: s"com.disneystreaming.smithy4s:smithy4s-codegen-cli_2.13:${BuildInfo.smithy4sVersion}" :: "--" :: args)
              .mkString(" "),
            ExecSyncOptions().setCwd(cwd),
          )
        }
      }
      .map { process =>
        (process: Any @unchecked) match {
          case s: String => s
          case b         => b.asInstanceOf[typings.node.Buffer].toString(BufferEncoding.utf8)
        }
      }

  }

}
