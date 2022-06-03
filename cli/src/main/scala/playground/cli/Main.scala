package playground.cli

import cats.effect.ExitCode
import cats.effect.IO
import cats.implicits._
import com.monovore.decline.Argument
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import fs2.io.file
import org.http4s.Uri

object Main extends CommandIOApp("smithyql", "SmithyQL CLI") with PlatformMain {

  private implicit val uriArgument: Argument[Uri] =
    Argument.from("uri")(s => Uri.fromString(s).leftMap(_.toString()).toValidatedNel)

  private implicit val fs2PathArgument: Argument[file.Path] = Argument[String].map(
    file.Path(_)
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

  def main: Opts[IO[ExitCode]] =
    serveSubcommand <+>
      Opts.subcommand("run", "Run query")(run) <+>
      Opts.subcommand("format", "Format query")(fmt) <+>
      Opts.subcommand("compile", "Compile query")(compile) <+>
      Opts.subcommand("info", "Show information about the current context")(info)

}
