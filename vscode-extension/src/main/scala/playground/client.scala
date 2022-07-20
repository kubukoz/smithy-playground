package playground

import cats.effect.Resource
import cats.effect.implicits._
import cats.effect.kernel.Async
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import fs2.io.net.tls.TLSContext
import fs2.io.net.tls.SecureContext
import org.http4s.client.middleware.Logger
import cats.effect.std

object client {

  def make[F[_]: Async: std.Console]: Resource[F, Client[F]] = Async[F]
    .delay(
      // todo: use facade
      TLSContext
        .Builder
        .forAsync[F]
        .fromSecureContext(
          SecureContext
            .fromJS(
              scalajs
                .js
                .Dynamic
                .global
                .require("tls")
                .applyDynamic("createSecureContext")(
                  scalajs
                    .js
                    .Object
                    .fromEntries(
                      scalajs
                        .js
                        .Array(
                          scalajs
                            .js
                            .Tuple2(
                              "_vscodeAdditionalCaCerts",
                              scalajs
                                .js
                                .Array
                                .apply(),
                            )
                        )
                    )
                )
            )
        )
    )
    .toResource
    .flatMap { tls =>
      EmberClientBuilder.default[F].withTLSContext(tls).build
    }
    .map(
      Logger[F](
        logHeaders = true,
        logBody = true,
        logAction = Some(std.Console[F].println(_: String)),
      )
    )

}
