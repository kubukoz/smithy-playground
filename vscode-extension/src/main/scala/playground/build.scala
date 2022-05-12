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

object build {

  val configFiles = List("build/smithy-dependencies.json", ".smithy.json", "smithy-build.json")

  def buildFile[F[_]: Async](
    chan: mod.OutputChannel
  ): F[BuildConfig] = fs2
    .Stream
    .exec(
      Sync[F].delay(chan.appendLine(s"Loading config from ${configFiles.mkString(", ")}..."))
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
      Async[F].fromFuture {
        Sync[F].delay {
          mod
            .workspace
            .openTextDocument(uri)
            .asInstanceOf[js.Thenable[mod.TextDocument]]
            .toFuture
        }
      }
    }
    .map(_.getText())
    .head
    .compile
    .lastOrError
    .flatTap { _ =>
      Sync[F].delay(chan.appendLine("Parsing config..."))
    }
    .map { s =>
      val parsed = js.JSON.parse(s)

      val deps =
        parsed
          .mavenDependencies
          .asInstanceOf[js.UndefOr[js.Array[String]]]
          .getOrElse(js.Array())
          .toList

      val repos =
        parsed
          .mavenRepositories
          .asInstanceOf[js.UndefOr[js.Array[String]]]
          .getOrElse(js.Array())
          .toList

      val imports =
        parsed
          .imports
          .asInstanceOf[js.UndefOr[js.Array[String]]]
          .getOrElse(js.Array())
          .toList

      BuildConfig(deps, repos, imports)
    }

  def getServices(
    buildFile: BuildConfig,
    chan: mod.OutputChannel,
  ): DynamicSchemaIndex =
    debug.timed("getService") {
      chan.appendLine("Dumping model...")

      val repos = buildFile
        .repos
        .toNel
        .foldMap(repos => "--repositories" :: repos.mkString_(",") :: Nil)
      val deps = buildFile
        .deps
        .toNel
        .foldMap(deps => "--dependencies" :: deps.mkString_(",") :: Nil)

      val args =
        "dump-model" ::
          buildFile.imports :::
          repos :::
          deps

      val process =
        debug.timed("dump-model") {
          nodeChildProcessMod.execSync(
            // todo: pass version from workspace config, default from sbt-buildinfo
            ("cs" :: "launch" :: s"com.disneystreaming.smithy4s:smithy4s-codegen-cli_2.13:${BuildInfo.smithy4sVersion}" :: "--" :: args)
              .mkString(" ")
          )
        }

      val modelText =
        (process: Any @unchecked) match {
          case s: String => s
          case b         => b.asInstanceOf[typings.node.Buffer].toString(BufferEncoding.utf8)
        }

      chan.appendLine("Parsing model...")

      val decodedModel = debug.timed("parse-model")(ModelReader.modelParser(modelText))

      chan.appendLine("Loading schemas...")

      val services =
        debug
          .timed("DSI.load") {
            ModelReader.buildSchemaIndex(decodedModel)
          }

      chan.appendLine(
        "Loaded services: " + services.allServices.map(_.service.id.show).mkString(", ") + "\n\n"
      )

      services
    }

}
