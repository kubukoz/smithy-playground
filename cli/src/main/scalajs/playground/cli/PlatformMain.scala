package playground.cli

import com.monovore.decline.Opts
import cats.effect.IO
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.implicits._
import smithy4s.http4s.SimpleRestJsonBuilder
import cats.effect.kernel.Resource
import cats.effect.ExitCode

//js
trait PlatformMain {

  protected val serverOpt: Opts[Boolean] = Opts(true)

  protected def impl(
    ignored: Boolean
  ): Resource[IO, CliService[IO]] = EmberClientBuilder
    .default[IO]
    .build
    .flatMap { client =>
      SimpleRestJsonBuilder(CliService).clientResource(client, uri"http://localhost:4200")
    }

  protected val serveSubcommand: Opts[IO[ExitCode]] = Opts.never

}
