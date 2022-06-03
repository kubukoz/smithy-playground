package playground.cli

import com.monovore.decline.Opts
import cats.effect.IO
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.implicits._
import smithy4s.http4s.SimpleRestJsonBuilder
import cats.effect.kernel.Resource
import cats.effect.ExitCode
import org.http4s.ember.server.EmberServerBuilder

//jvm
trait PlatformMain {

  protected val serverOpt: Opts[Boolean] =
    Opts
      .flag(
        "server",
        "Whether the program should use the local server or execute the command directly",
      )
      .orFalse

  protected def impl(server: Boolean): Resource[IO, CliService[IO]] =
    if (server)
      EmberClientBuilder
        .default[IO]
        .build
        .flatMap { client =>
          SimpleRestJsonBuilder(CliService).clientResource(client, uri"http://localhost:4200")
        }
    else
      Resource.pure[IO, CliService[IO]](CommandLineRunner.instance)

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

  protected val serveSubcommand: Opts[IO[ExitCode]] =
    Opts.subcommand("serve", "Start server")(serve)
}
