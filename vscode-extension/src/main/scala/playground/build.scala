package playground

import cats.effect.kernel.Async
import cats.effect.kernel.Sync
import cats.implicits._
import io.scalajs.nodejs.child_process.ChildProcess
import smithy4s.SchemaIndex
import smithy4s.api.SimpleRestJson
import smithy4s.dynamic.DynamicSchemaIndex
import smithy4s.dynamic.model.Model
import typings.vscode.mod

import scala.scalajs.js.JSConverters._

import scalajs.js
import scala.concurrent.duration._

object build {

  val configFiles = List("build/smithy-dependencies.json", ".smithy.json", "smithy-build.json")

  def buildFile[F[_]: Async](
    chan: mod.OutputChannel
  ): F[BuildInfo] = fs2
    .Stream
    .emits(configFiles)
    .append(
      fs2
        .Stream
        .exec(
          Sync[F].delay(chan.appendLine(s"Loading config from ${configFiles.mkString(", ")}..."))
        )
    )
    .evalTap(_ => Async[F].sleep(1.second))
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

      BuildInfo(deps, repos, imports)
    }

  case class BuildInfo(deps: List[String], repos: List[String], imports: List[String])

  def getService(
    buildFile: BuildInfo,
    chan: mod.OutputChannel,
  ): DynamicSchemaIndex.ServiceWrapper = {
    chan.appendLine("Dumping model...")

    val repos = buildFile
      .repos
      .toNel
      .foldMap(repos => "--repositories" :: repos.mkString_(",") :: Nil)
    val deps = buildFile.deps.toNel.foldMap(deps => "--dependencies" :: deps.mkString_(",") :: Nil)

    val args =
      "dump-model" ::
        buildFile.imports :::
        repos :::
        deps

    val process = ChildProcess.execFileSync(
      "/nix/store/m5igl1nk1wblx5alzj8r2l56awnwgyvk-smithy4s-codegen-0.12.7/bin/smithy4s-codegen",
      args.toJSArray,
    )

    val modelText =
      (process: Any @unchecked) match {
        case b: io.scalajs.nodejs.buffer.Buffer => b.toString("UTF-8")
        case s: String                          => s
      }

    chan.appendLine("Parsing model...")

    val capi = smithy4s.http.json.codecs()

    val decodedModel =
      capi.decodeFromByteArray(capi.compileCodec(Model.schema), modelText.getBytes()).toTry.get

    chan.appendLine("Loading schemas...")

    val services =
      DynamicSchemaIndex
        .load(
          decodedModel,
          SimpleRestJson
            .protocol
            .schemas ++
            // todo: should be included
            SchemaIndex(
              SimpleRestJson,
              smithy.api.Error,
              smithy.api.Documentation,
              smithy.api.ExternalDocumentation,
              smithy.api.Deprecated,
            ),
        )
        .allServices

    chan.appendLine("Loaded services: " + services.map(_.service.id.show).mkString(", ") + "\n\n")

    services.head
  }

}
