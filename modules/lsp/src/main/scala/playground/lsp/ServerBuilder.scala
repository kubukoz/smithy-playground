package playground.lsp

import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.effect.std
import cats.effect.std.Supervisor
import cats.implicits._
import org.http4s.client.Client
import org.http4s.client.middleware.Logger
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.Authorization
import playground.language.CommandResultReporter
import playground.OperationRunner
import playground.TextDocumentManager
import playground.std.StdlibRuntime
import smithy4s.aws.AwsEnvironment
import smithy4s.aws.http4s.AwsHttp4sBackend
import smithy4s.aws.kernel.AwsRegion

trait ServerBuilder[F[_]] {
  def build(buildInfo: BuildLoader.Loaded, loader: ServerLoader[F]): F[LanguageServer[F]]
}

object ServerBuilder {
  def apply[F[_]](implicit F: ServerBuilder[F]): ServerBuilder[F] = F

  def instance[F[_]: Async: LanguageClient: BuildLoader: std.Console] = {
    implicit val pluginResolver: PluginResolver[F] = PluginResolver.instance[F]

    implicit val stdlibRuntime: StdlibRuntime[F] = StdlibRuntime.instance[F]

    EmberClientBuilder
      .default[F]
      .build
      .map(middleware.AuthorizationHeader[F])
      .map(
        Logger[F](
          logHeaders = true,
          logBody = true,
          logAction = Some(msg =>
            LanguageClient[F].logOutput(msg.linesWithSeparators.map("// " + _).mkString)
          ),
        )
      )
      .flatMap { client =>
        AwsEnvironment
          .default(AwsHttp4sBackend(client), AwsRegion.US_EAST_1)
          .memoize
          .flatMap { awsEnv =>
            TextDocumentManager
              .instance[F]
              .toResource
              .flatMap { implicit tdm =>
                Supervisor[F].map { implicit sup =>
                  new ServerBuilder[F] {

                    def build(buildInfo: BuildLoader.Loaded, loader: ServerLoader[F])
                      : F[LanguageServer[F]] = BuildLoader[F]
                      .buildSchemaIndex(buildInfo)
                      .flatMap { dsi =>
                        PluginResolver[F]
                          .resolveFromConfig(buildInfo.config)
                          .flatMap { plugins =>
                            val runner = OperationRunner
                              .forSchemaIndex[F](
                                dsi,
                                client,
                                LanguageClient[F]
                                  .configuration(ConfigurationValue.baseUri),
                                awsEnv,
                                plugins = plugins,
                              )

                            implicit val sl: ServerLoader[F] = loader

                            CommandResultReporter.instance[F].map { implicit rep =>
                              LanguageServer.instance[F](dsi, runner)
                            }
                          }
                      }
                  }
                }
              }
          }
      }
  }

  private object middleware {

    def AuthorizationHeader[F[_]: Async: LanguageClient]: Client[F] => Client[F] =
      client =>
        Client[F] { request =>
          val updatedRequest =
            LanguageClient[F]
              .configuration(ConfigurationValue.authorizationHeader)
              .flatMap {
                case v if v.trim.isEmpty() => request.pure[F]
                case v => Authorization.parse(v).liftTo[F].map(request.putHeaders(_))
              }
              .toResource

          updatedRequest
            .flatMap(client.run(_))
        }

  }

}
