package playground.cli

import cats.effect.IO
import cats.effect.implicits._
import cats.effect.kernel.Resource
import cats.effect.std
import cats.implicits._
import fs2.io.file
import fs2.io.file.Files
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.implicits._
import playground.BuildConfig
import playground.BuildConfigDecoder
import playground.ModelReader
import playground.Runner
import playground.cli.CliService
import playground.smithyql.Formatter
import playground.smithyql.SmithyQLParser
import playground.smithyql.WithSource
import smithy4s.Document
import smithy4s.aws.AwsEnvironment
import smithy4s.aws.http4s.AwsHttp4sBackend
import smithy4s.aws.kernel.AwsRegion
import smithy4s.codegen.cli.DumpModel
import smithy4s.codegen.cli.Smithy4sCommand
import smithy4s.dynamic.DynamicSchemaIndex

object CommandLineRunner {

  val instance: Resource[IO, CliService[IO]] = EmberClientBuilder
    .default[IO]
    .build
    // .map(
    //   middleware.Logger[IO](
    //     logHeaders = true,
    //     logBody = true,
    //     logAction = Some(IO.println(_: String)),
    //   )
    // )
    .flatMap { client =>
      AwsEnvironment
        .default(AwsHttp4sBackend(client), AwsRegion.US_EAST_1)
        .memoize
        .map { awsEnv =>
          new CliService[IO] {

            def run(
              input: String,
              baseUri: playground.cli.Url,
              width: Option[Int],
              context: Option[String],
            ): IO[RunOutput] = runImpl(
              file.Path(input),
              Uri.unsafeFromString(baseUri.value),
              width.getOrElse(80),
              context.fold(file.Path("."))(file.Path(_)),
            )(client, awsEnv).map(RunOutput.apply)

            def format(
              input: String,
              width: Option[Int],
            ): IO[FormatOutput] = loadAndParse(file.Path(input))
              .map { parsed =>
                FormatOutput(Formatter.format(parsed, width.getOrElse(80)))
              }

            def compile(input: String, context: Option[String]): IO[CompileOutput] = compileImpl(
              file.Path(input),
              context.fold(file.Path("."))(file.Path(_)),
            ).map(CompileOutput.apply)

            def info(context: Option[String]): IO[InfoOutput] = readBuildConfig(
              context.fold(file.Path("."))(file.Path(_))
            )
              .flatMap { bc =>
                buildSchemaIndex(bc)
                  .map { dsi =>
                    InfoOutput(
                      s"Build config:\n$bc\nDiscovered services:\n" + dsi
                        .allServices
                        .map(svc => s"${svc.service.id} (${svc.service.endpoints.size} operations)")
                        .mkString("\n")
                    )
                  }
              }

          }

        }
    }

  private def compileImpl(input: file.Path, ctx: file.Path) = loadAndParse(input)
    .flatMap { parsed =>
      readBuildConfig(ctx)
        .flatMap(buildSchemaIndex)
        .flatMap { dsi =>
          val compiler = playground.Compiler.fromSchemaIndex[IO](dsi)

          val runner = Runner
            .forSchemaIndex[IO](dsi, Client(_ => Resource.never), IO.never, Resource.never)
            .get(parsed)

          val runnerStatus = runner
            .toEither
            .fold(
              issues => s"unavailable ($issues)",
              _ => "available",
            )

          compiler.compile(parsed).map { cin =>
            val input = Document.Encoder.fromSchema(cin.endpoint.input).encode(cin.input)

            s"""Service: ${cin.serviceId.render}
               |Endpoint: ${cin.endpoint.id}
               |Compiled input: $input
               |Runner status: $runnerStatus""".stripMargin
          }
        }
    }

  private def runImpl(
    filePath: file.Path,
    baseUri: Uri,
    width: Int,
    ctx: file.Path,
  )(
    client: Client[IO],
    awsEnv: Resource[IO, AwsEnvironment[IO]],
  ): IO[String] = loadAndParse(
    filePath
  )
    .flatMap { parsed =>
      readBuildConfig(ctx)
        .flatMap(buildSchemaIndex)
        .flatMap { dsi =>
          val compiler = playground.Compiler.fromSchemaIndex[IO](dsi)

          Runner
            .forSchemaIndex[IO](dsi, client, baseUri.pure[IO], awsEnv)
            .get(parsed)
            .toEither
            .leftMap { issues =>
              new Throwable("Cannot build runner: " + issues)
            }
            .liftTo[IO]
            .flatMap { runner =>
              compiler.compile(parsed).flatMap { compiled =>
                runner
                  .run(compiled)
                  .map { result =>
                    Formatter
                      .writeAst(result.mapK(WithSource.liftId))
                      .renderTrim(width)
                  }
                  .onError { e =>
                    compiled
                      .catchError(e)
                      .flatMap(ee => compiled.writeError.map(_.toNode(ee)))
                      .map { result =>
                        std
                          .Console[IO]
                          .errorln(
                            "ERROR: " + Formatter
                              .writeAst(result.mapK(WithSource.liftId))
                              .renderTrim(width)
                          )
                      }
                      .getOrElse(IO.raiseError(e))
                  }

              }
            }
        }
    }

  private def loadAndParse(filePath: file.Path) = Files[IO]
    .readAll(filePath)
    .through(fs2.text.utf8.decode[IO])
    .compile
    .string
    .flatMap { queryString =>
      SmithyQLParser.parseFull(queryString).liftTo[IO]
    }

  private def readBuildConfig(ctx: file.Path) = Files[IO]
    .readAll(ctx / "smithy-build.json")
    .compile
    .toVector
    .map(_.toArray)
    .flatMap {
      BuildConfigDecoder.decode(_).liftTo[IO]
    }

  private def buildSchemaIndex(bc: BuildConfig): IO[DynamicSchemaIndex] = IO
    .interruptibleMany {
      DumpModel.run(
        Smithy4sCommand.DumpModelArgs(
          specs = bc.imports.combineAll.map(os.Path(_)),
          repositories = bc.mavenRepositories.combineAll,
          dependencies = bc.mavenDependencies.combineAll,
          transformers = Nil,
        )
      )
    }
    .flatMap { modelText =>
      Either
        .catchNonFatal(ModelReader.modelParser(modelText))
        .liftTo[IO]
        .map(ModelReader.buildSchemaIndex(_))
    }

}
