package playground.lsp

import cats.effect.implicits.*
import cats.effect.kernel.Async
import cats.effect.kernel.MonadCancelThrow
import cats.effect.kernel.Resource
import cats.effect.std
import cats.syntax.all.*
import fs2.compression.Compression
import fs2.io.file.Files
import fs2.io.net.Network
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.client.middleware.Logger
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.Authorization
import org.http4s.implicits.*
import playground.FileRunner
import playground.OperationRunner
import playground.ServiceIndex
import playground.TextDocumentManager
import playground.language.CommandResultReporter
import playground.std.StdlibRuntime
import smithy4s.aws.AwsEnvironment
import smithy4s.aws.kernel.AwsRegion

trait ServerBuilder[F[_]] {

  def build(
    buildInfo: BuildLoader.Loaded,
    loader: ServerLoader[F],
  ): F[LanguageServer[F]]

}

object ServerBuilder {

  def apply[F[_]](
    implicit F: ServerBuilder[F]
  ): ServerBuilder[F] = F

  def instance[F[_]: Async: LanguageClient: BuildLoader: Files: Network: Compression: std.Console]
    : Resource[F, ServerBuilder[F]] = {
    implicit val pluginResolver: PluginResolver[F] = PluginResolver.instance[F]

    implicit val stdlibRuntime: StdlibRuntime[F] = StdlibRuntime.instance[F]

    val logger = Logger[F](
      logHeaders = true,
      logBody = true,
      logAction = Some(msg =>
        LanguageClient[F].logOutput(msg.linesWithSeparators.map("// " + _).mkString)
      ),
    )

    val makeClient = EmberClientBuilder
      .default[F]
      .build
      .map(
        // logger gets applied first, so that it sees the final result
        identity[Client[F]]
          .compose(middleware.BaseUri[F])
          .compose(middleware.AuthorizationHeader[F])
          .compose(logger)
      )

    for {
      client <- makeClient
      awsEnv <-
        AwsEnvironment
          .default(client, AwsRegion.US_EAST_1)
          .memoize
      tdm <- TextDocumentManager.instance[F].toResource
    } yield new ServerBuilder[F] {
      private implicit val textManager: TextDocumentManager[F] = tdm

      def build(
        buildInfo: BuildLoader.Loaded,
        loader: ServerLoader[F],
      ): F[LanguageServer[F]] =
        for {
          dsi <- BuildLoader[F].buildSchemaIndex(buildInfo)
          plugins <- PluginResolver[F].resolve(buildInfo.config)
          rep <- CommandResultReporter.instance[F]
        } yield {
          val runners = OperationRunner
            .forSchemaIndex[F](
              dsi = dsi,
              client = client,
              awsEnv = awsEnv,
              plugins = plugins,
            )

          val serviceIndex = ServiceIndex.fromServices(dsi.allServices.toList)

          implicit val sl: ServerLoader[F] = loader

          implicit val reporter: CommandResultReporter[F] = rep

          LanguageServer
            .instance[F](dsi, FileRunner.instance(OperationRunner.merge[F](runners, serviceIndex)))
        }
    }
  }

  private object middleware {

    def BaseUri[F[_]: LanguageClient: MonadCancelThrow]: Client[F] => Client[F] =
      client =>
        Client[F] {
          case req if req.uri != uri"/" => client.run(req)
          case req =>
            LanguageClient[F].configuration(ConfigurationValue.baseUri).toResource.flatMap {
              case None => client.run(req)
              case Some(uri) =>
                client.run(
                  req
                    .withUri(
                      req
                        .uri
                        .copy(
                          scheme = uri.scheme,
                          authority = uri.authority,
                          // prefixing with uri.path
                          path = uri.path.addSegments(req.uri.path.segments),
                        )
                    )
                )
            }
        }

    def AuthorizationHeader[F[_]: LanguageClient: MonadCancelThrow]: Client[F] => Client[F] =
      client =>
        Client[F] { request =>
          val updatedRequest =
            LanguageClient[F]
              .configuration(ConfigurationValue.authorizationHeader)
              .flatMap {
                case None    => request.pure[F]
                case Some(v) => Authorization.parse(v).liftTo[F].map(request.putHeaders(_))
              }
              .toResource

          updatedRequest
            .flatMap(client.run(_))
        }

  }

}
