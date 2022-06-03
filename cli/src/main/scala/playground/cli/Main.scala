package playground.cli

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.implicits._
import com.monovore.decline.Argument
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import fs2.io.file
import org.http4s.Uri
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import playground.cli.CliService
import smithy4s.http4s.SimpleRestJsonBuilder

import java.nio.file.Path

object Main extends CommandIOApp("smithyql", "SmithyQL CLI") {

  private implicit val uriArgument: Argument[Uri] =
    Argument.from("uri")(s => Uri.fromString(s).leftMap(_.toString()).toValidatedNel)

  private implicit val fs2PathArgument: Argument[file.Path] = Argument[Path].map(
    file.Path.fromNioPath
  )

  private val widthOpt = Opts
    .option[Int]("width", "Max width of formatted output (not enforced)")
    .withDefault(80)

  private val ctxOpt = Opts
    .option[file.Path](
      "ctx",
      "Context (location of smithy-build.json will be resolved against this path)",
    )
    .withDefault(file.Path("."))

  private val serverOpt =
    Opts
      .flag(
        "server",
        "Whether the program should use the local server or execute the command directly",
      )
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
      Resource.pure[IO, CliService[IO]](CommandLineRunner.instance)

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

  private val fmt =
    (
      Opts
        .argument[file.Path]("input"),
      widthOpt,
      serverOpt,
    )
      .mapN { (filePath, width, server) =>
        impl(server)
          .use(_.format(filePath.toString, width.some))
          .map(_.response)
          .flatMap(IO.println(_))
          .as(ExitCode.Success)
      }

  private val info = (ctxOpt, serverOpt).mapN { (ctx, server) =>
    impl(server)
      .use(_.info(ctx.toString.some))
      .map(_.response)
      .flatMap(IO.println(_))
      .as(ExitCode.Success)
  }

  private val serve: Opts[IO[ExitCode]] = Opts {
    import com.comcast.ip4s._

    SimpleRestJsonBuilder
      .routes(CommandLineRunner.instance)
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
