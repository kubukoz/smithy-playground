package playground.cli

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.implicits._
import cats.implicits._
import com.monovore.decline.Argument
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import fs2.io.file
import fs2.io.file.Files
import org.http4s.Uri
import org.http4s.ember.client.EmberClientBuilder
import playground.BuildConfig
import playground.ModelReader
import playground.Runner
import playground.smithyql.Formatter
import playground.smithyql.SmithyQLParser
import playground.smithyql.WithSource
import smithy4s.Document
import smithy4s.aws.AwsEnvironment
import smithy4s.aws.http4s.AwsHttp4sBackend
import smithy4s.aws.kernel.AwsRegion
import smithy4s.codegen.cli.DumpModel
import smithy4s.codegen.cli.Smithy4sCommand

import java.nio.file.Path
import smithy4s.dynamic.DynamicSchemaIndex
import org.http4s.client.Client
import cats.effect.kernel.Resource
import cats.effect.std
import playground.BuildConfigDecoder

object Main extends CommandIOApp("smithyql", "SmithyQL CLI") {

  private implicit val uriArgument: Argument[Uri] =
    Argument.from("uri")(s => Uri.fromString(s).leftMap(_.toString()).toValidatedNel)

  private implicit val fs2PathArgument: Argument[file.Path] = Argument[Path].map(
    file.Path.fromNioPath
  )

  private def loadAndParse(filePath: Path) = Files[IO]
    .readAll(file.Path.fromNioPath(filePath))
    .through(fs2.text.utf8.decode[IO])
    .compile
    .string
    .flatMap { queryString =>
      SmithyQLParser.parseFull(queryString).liftTo[IO]
    }

  private val widthOpt = Opts
    .option[Int]("width", "Max width of formatted output (not enforced)")
    .withDefault(80)

  private val fmt =
    (
      Opts
        .argument[Path]("input"),
      widthOpt,
    )
      .mapN { (filePath, width) =>
        loadAndParse(filePath)
          .flatMap { parsed =>
            IO.println(Formatter.format(parsed, width))
          }
          .as(ExitCode.Success)
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

  private val ctxOpt = Opts
    .option[file.Path](
      "ctx",
      "Context (location of smithy-build.json will be resolved against this path)",
    )
    .withDefault(file.Path("."))

  private val compile =
    (
      Opts.argument[Path]("input"),
      ctxOpt,
    ).mapN { (filePath, ctx) =>
      loadAndParse(filePath)
        .flatMap { parsed =>
          readBuildConfig(ctx)
            .flatMap(buildSchemaIndex)
            .flatMap { dsi =>
              val compiler = playground.Compiler.fromSchemaIndex(dsi)

              val runner = Runner
                .forSchemaIndex[IO](dsi, Client(_ => Resource.never), IO.never, Resource.never)
                .get(parsed)

              val runnerStatus = runner
                .toEither
                .fold(
                  issues => s"unavailable ($issues)",
                  _ => "available",
                )

              compiler.compile(parsed).toEither.liftTo[IO].flatMap { cin =>
                val input = Document.Encoder.fromSchema(cin.endpoint.input).encode(cin.input)

                IO.println(
                  s"""Service: ${cin.serviceId.render}
                   |Endpoint: ${cin.endpoint.id}
                   |Compiled input: $input
                   |Runner status: $runnerStatus""".stripMargin
                )
              }
            }
        }
        .as(ExitCode.Success)
    }

  private val run =
    (
      Opts.argument[Path]("input"),
      Opts.argument[Uri]("base-uri"),
      widthOpt,
      ctxOpt,
    ).mapN { (filePath, baseUri, width, ctx) =>
      loadAndParse(filePath)
        .flatMap { parsed =>
          readBuildConfig(ctx)
            .flatMap(buildSchemaIndex)
            .flatMap { dsi =>
              val compiler = playground.Compiler.fromSchemaIndex(dsi)

              EmberClientBuilder.default[IO].build.use { client =>
                AwsEnvironment
                  .default(AwsHttp4sBackend(client), AwsRegion.US_EAST_1)
                  .memoize
                  .use { awsEnv =>
                    Runner
                      .forSchemaIndex[IO](dsi, client, baseUri.pure[IO], awsEnv)
                      .get(parsed)
                      .toEither
                      .leftMap { issues =>
                        new Throwable("Cannot build runner: " + issues)
                      }
                      .liftTo[IO]
                      .flatMap { runner =>
                        compiler.compile(parsed).toEither.liftTo[IO].flatMap { compiled =>
                          runner
                            .run(compiled)
                            .flatMap { result =>
                              IO.println(
                                Formatter
                                  .writeAst(result.mapK(WithSource.liftId))
                                  .renderTrim(width)
                              )
                            }
                            .handleErrorWith { e =>
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
            }
        }
        .as(ExitCode.Success)
    }

  val info = ctxOpt.map { ctx =>
    readBuildConfig(ctx)
      .flatMap { bc =>
        IO.println(s"Build config:\n$bc\n") *>
          buildSchemaIndex(bc)
            .flatMap { dsi =>
              IO.println(
                "Discovered services:\n" + dsi
                  .allServices
                  .map(svc => s"${svc.service.id} (${svc.service.endpoints.size} operations)")
                  .mkString("\n")
              )
            }
      }
      .as(ExitCode.Success)
  }

  def main: Opts[IO[ExitCode]] =
    Opts.subcommand("run", "Run query")(run) <+>
      Opts.subcommand("format", "Format query")(fmt) <+>
      Opts.subcommand("compile", "Compile query")(compile) <+>
      Opts.subcommand("info", "Show information about the current context")(info)

}
