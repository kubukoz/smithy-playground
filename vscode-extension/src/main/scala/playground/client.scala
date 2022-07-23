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
import cats.implicits._
import org.http4s.headers.Authorization

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
    .map(AuthorizationHeader[F])
    .map(
      Logger[F](
        logHeaders = true,
        logBody = true,
        logAction = Some(std.Console[F].println(_: String)),
      )
    )

  private def AuthorizationHeader[F[_]: Async]: Client[F] => Client[F] =
    client =>
      Client[F] { request =>
        val updatedRequest =
          vscodeutil
            .getConfigF[F, String]("smithyql.http.authorizationHeader")
            .flatMap {
              case v if v.trim.isEmpty() => request.pure[F]
              case v => Authorization.parse(v).liftTo[F].map(request.putHeaders(_))
            }
            .toResource

        updatedRequest
          .flatMap(client.run(_))
      }

}
