package playground.lsp

import cats.data.NonEmptyList
import cats.effect.implicits.*
import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import cats.effect.std
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import fs2.compression.Compression
import fs2.io.file.Files
import fs2.io.net.Network
import org.http4s.client.Client
import org.http4s.client.middleware.Logger
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.Authorization
import playground.FileRunner
import playground.Interpreters
import playground.OperationRunner
import playground.ServiceIndex
import playground.language.CommandResultReporter
import playground.plugins.Environment
import playground.plugins.Interpreter
import playground.plugins.SimpleHttpBuilder
import playground.std.StdlibRuntime
import smithy4s.aws.AwsEnvironment
import smithy4s.aws.kernel.AwsRegion
import smithy4s.http4s.SimpleRestJsonBuilder

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

  def instance[
    F[_]: Async: LanguageClient: BuildLoader: Files: Network: Compression: std.Console: UUIDGen
  ]: Resource[F, ServerBuilder[F]] = {
    implicit val pluginResolver: PluginResolver[F] = PluginResolver.instance[F]

    implicit val stdlibRuntime: StdlibRuntime[F] = StdlibRuntime.instance[F]

    for {
      httpClient <- EmberClientBuilder
        .default[F]
        .build
        .map(middleware.AuthorizationHeader[F])
        .map(
          Logger[F](
            logHeaders = true,
            logBody = true,
            logAction = Some { msg =>
              LanguageClient[F].logOutput(msg.linesWithSeparators.map("// " + _).mkString)
            },
          )
        )

      awsEnv <-
        AwsEnvironment
          .default(httpClient, AwsRegion.US_EAST_1)
          .memoize
      given TextDocumentManager[F] <- TextDocumentManager.instance[F].toResource
      rep <- CommandResultReporter.instance[F].toResource

    } yield new ServerBuilder[F] {
      given Environment[F] =
        new {
          def getK(k: Environment.Key): Option[k.Value[F]] = {
            val yolo: Option[Any] =
              k match {
                case Environment.httpClient => Some(httpClient)
                case Environment.baseUri =>
                  Some(LanguageClient[F].configuration(ConfigurationValue.baseUri))
                case Environment.console => Some(std.Console[F])
                case _                   => None
              }
            yolo.map(_.asInstanceOf[k.Value[F]])
          }

        }

      def build(
        buildInfo: BuildLoader.Loaded,
        loader: ServerLoader[F],
      ): F[LanguageServer[F]] =
        for {
          dsi <- BuildLoader[F].buildSchemaIndex(buildInfo)
          plugins <- PluginResolver[F].resolve(buildInfo.config)
        } yield {
          val interpreters = NonEmptyList
            .of(
              Interpreter.fromSimpleBuilder(
                SimpleHttpBuilder.fromSimpleProtocolBuilder(SimpleRestJsonBuilder)
              ),
              Interpreters.aws(awsEnv),
              Interpreters.stdlib[F],
            )
            .concat(plugins.flatMap(_.interpreters))

          val runners = OperationRunner
            .forServices[F](
              services = dsi.allServices.toList,
              getSchema = dsi.getSchema,
              interpreters = interpreters,
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
