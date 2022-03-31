package playground

import cats.effect.kernel.Async
import cats.effect.kernel.Sync
import typings.vscode.mod
import scalajs.js
import cats.implicits._
import smithy4s.dynamic.DynamicSchemaIndex
import io.scalajs.nodejs.child_process.ChildProcess
import smithy4s.api.SimpleRestJson
import smithy4s.dynamic.model.Model
import smithy4s.SchemaIndex
import scala.scalajs.js.JSConverters._

object build {

  def buildFile[F[_]: Async]: F[BuildInfo] = fs2
    .Stream("build/smithy-dependencies.json", ".smithy.json", "smithy-build.json")
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

  def getService(buildFile: BuildInfo): DynamicSchemaIndex.ServiceWrapper = {
    val repos = buildFile.repos.toNel.foldMap(repos => "--repositories" :: repos.toList)
    val deps = buildFile.deps.toNel.foldMap(deps => "--dependencies" :: deps.toList)

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

    val capi = smithy4s.http.json.codecs()

    val services =
      DynamicSchemaIndex
        .load(
          capi.decodeFromByteArray(capi.compileCodec(Model.schema), modelText.getBytes()).toTry.get,
          SimpleRestJson
            .protocol
            .schemas ++
            // todo: should be included
            SchemaIndex(SimpleRestJson),
        )
        .allServices

    println("services: " + services.head.service.id)
    services.head
  }

}
