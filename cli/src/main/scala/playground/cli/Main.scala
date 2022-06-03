package playground.cli

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.implicits._
import cats.effect.kernel.Resource
import cats.effect.std
import cats.implicits._
import com.monovore.decline.Argument
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import fs2.io.file
import fs2.io.file.Files
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
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
import smithy4s.http4s.SimpleRestJsonBuilder

import java.nio.file.Path

object Main extends CommandIOApp("smithyql", "SmithyQL CLI") {

  private implicit val uriArgument: Argument[Uri] =
    Argument.from("uri")(s => Uri.fromString(s).leftMap(_.toString()).toValidatedNel)

  private implicit val fs2PathArgument: Argument[file.Path] = Argument[Path].map(
    file.Path.fromNioPath
  )

  private def loadAndParse(filePath: file.Path) = Files[IO]
    .readAll(filePath)
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
        .argument[file.Path]("input"),
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

  private val serverOpt =
    Opts
      .flag("server", "Whether the query should use the local server or be executed directly")
      .orFalse

  private def impl(server: Boolean) =
    if (server)
      EmberClientBuilder
        .default[IO]
        .build
        .flatMap { client =>
          SimpleRestJsonBuilder(CliService).clientResource(client, uri"http://localhost:4200")
        }
    else
      Resource.pure[IO, CliService[IO]](cli)

  private val run =
    (
      Opts.argument[file.Path]("input"),
      Opts.argument[Uri]("base-uri"),
      widthOpt,
      ctxOpt,
      serverOpt,
    ).mapN((input, baseUri, width, ctx, server) =>
      impl(server)
        .use(_.run(input.toString, Url(baseUri.renderString), width.some, ctx.toString.some))
        .map(_.response)
        .flatMap(IO.println(_))
        .as(ExitCode.Success)
    )

  private val compile =
    (
      Opts.argument[file.Path]("input"),
      ctxOpt,
      serverOpt,
    ).mapN { (input, ctx, server) =>
      impl(server)
        .use(_.compile(input.toString, ctx.toString.some))
        .map(_.response)
        .flatMap(IO.println(_))
        .as(ExitCode.Success)
    }

  val cli: CliService[IO] =
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
      ).map(RunOutput.apply)

      def format(input: String): IO[Unit] = IO.unit

      def compile(input: String, context: Option[String]): IO[CompileOutput] = compileImpl(
        file.Path(input),
        context.fold(file.Path("."))(file.Path(_)),
      ).map(CompileOutput.apply)

      def info(): IO[Unit] = IO.unit

    }

  private def runImpl(filePath: file.Path, baseUri: Uri, width: Int, ctx: file.Path): IO[String] =
    loadAndParse(
      filePath
    )
      .flatMap { parsed =>
        readBuildConfig(ctx)
          .flatMap(buildSchemaIndex)
          .flatMap { dsi =>
            val compiler = playground.Compiler.fromSchemaIndex[IO](dsi)

            EmberClientBuilder
              .default[IO]
              .build
              // .map(
              //   middleware.Logger[IO](
              //     logHeaders = true,
              //     logBody = true,
              //     logAction = Some(IO.println(_: String)),
              //   )
              // )
              .use { client =>
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
          }
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

  val serve: Opts[IO[ExitCode]] = Opts {
    import com.comcast.ip4s._
    SimpleRestJsonBuilder
      .routes(cli)
      .resource
      .flatMap { routes =>
        EmberServerBuilder
          .default[IO]
          .withHttpApp(
            routes.orNotFound
              // .pipe(
              //   Logger.httpApp[IO](
              //     logHeaders = true,
              //     logBody = true,
              //     logAction = Some(IO.println(_: String)),
              //   )
              // )
          )
          .withHost(host"0.0.0.0")
          .withPort(port"4200")
          .build
      }
      .useForever
  }

  def main: Opts[IO[ExitCode]] =
    Opts.subcommand("serve", "Start server")(serve) <+>
      Opts.subcommand("run", "Run query")(run) <+>
      Opts.subcommand("format", "Format query")(fmt) <+>
      Opts.subcommand("compile", "Compile query")(compile) <+>
      Opts.subcommand("info", "Show information about the current context")(info)

}
