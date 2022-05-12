package playground.cli

import cats.data.NonEmptyList
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
import smithy4s.aws.AwsEnvironment
import smithy4s.aws.http4s.AwsHttp4sBackend
import smithy4s.aws.kernel.AwsRegion
import smithy4s.codegen.cli.DumpModel
import smithy4s.codegen.cli.Smithy4sCommand

import java.nio.file.Path
import smithy4s.internals.DocumentEncoder
import smithy4s.internals.SchematicDocumentDecoder
import smithy4s.internals.SchematicDocumentEncoder
import smithy4s.Document

object Main extends CommandIOApp("smithyql", "SmithyQL CLI") {

  implicit val uriArgument: Argument[Uri] =
    Argument.from("uri")(s => Uri.fromString(s).leftMap(_.toString()).toValidatedNel)

  def loadAndParse(filePath: Path) = Files[IO]
    .readAll(file.Path.fromNioPath(filePath))
    .through(fs2.text.utf8.decode[IO])
    .compile
    .string
    .flatMap { queryString =>
      SmithyQLParser.parseFull(queryString).liftTo[IO]
    }

  private val fmt = Opts
    .argument[Path]("input")
    .map { filePath =>
      loadAndParse(filePath)
        .flatMap { parsed =>
          IO.println(Formatter.format(parsed, 80))
        }
        .as(ExitCode.Success)
    }

  private val buildSchemaIndex = IO(
    // todo
    BuildConfig(
      deps = Nil,
      repos = Nil,
      imports = "/Users/kubukoz/projects/smithy-playground/core/src/main/smithy/demo.smithy" :: Nil,
    )
  )
    .flatMap { bc =>
      IO.interruptibleMany {
        DumpModel.run(
          Smithy4sCommand.DumpModelArgs(
            specs = bc.imports.map(os.Path(_)),
            repositories = bc.repos,
            dependencies = bc.deps,
            transformers = Nil,
          )
        )
      }
    }
    .flatMap { modelText =>
      Either
        .catchNonFatal(ModelReader.modelParser(modelText))
        .liftTo[IO]
        .map(ModelReader.buildSchemaIndex(_))
    }

  private val compile = Opts.argument[Path]("input").map { filePath =>
    loadAndParse(filePath)
      .flatMap { parsed =>
        buildSchemaIndex
          .flatMap { dsi =>
            val compiler = playground.Compiler.fromSchemaIndex[IO](dsi)

            compiler.compile(parsed).flatMap { cin =>
              val input = Document.Encoder.fromSchema(cin.endpoint.input).encode(cin.input)

              IO.println(
                s"""Service: ${cin.serviceId.render}
                   |Endpoint: ${cin.endpoint.id}
                   |Compiled input: $input""".stripMargin
              )
            }
          }
      }
      .as(ExitCode.Success)
  }

  private val run = (Opts.argument[Path]("input"), Opts.argument[Uri]("base-uri")).mapN {
    (filePath, baseUri) =>
      loadAndParse(filePath)
        .flatMap { parsed =>
          buildSchemaIndex
            .flatMap { dsi =>
              val compiler = playground.Compiler.fromSchemaIndex[IO](dsi)

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
                        issues match {
                          case NonEmptyList(Runner.Issue.Other(e), Nil) => e
                          case _                                        => ???
                        }
                      }
                      .liftTo[IO]
                      .flatMap { runner =>
                        compiler.compile(parsed).flatMap { compiled =>
                          runner
                            .run(compiled)
                            .flatMap { result =>
                              IO.println(
                                Formatter
                                  .writeAst(result.mapK(WithSource.liftId))
                                  .renderTrim(80)
                              )
                            }
                        }
                      }

                  }
              }
            }
        }
        .as(ExitCode.Success)
  }

  def main: Opts[IO[ExitCode]] =
    Opts.subcommand("run", "Run query")(run) <+>
      Opts.subcommand("format", "Format query")(fmt) <+>
      Opts.subcommand("compile", "Compile query")(compile)

}
