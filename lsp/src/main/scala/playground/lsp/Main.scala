package playground.lsp

import cats.Show
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.effect.kernel.Deferred
import cats.effect.kernel.DeferredSink
import cats.effect.kernel.Resource
import cats.effect.kernel.Sync
import cats.effect.std
import cats.effect.std.Dispatcher
import cats.implicits._
import fs2.io.file.Path
import io.circe.Decoder
import org.eclipse.lsp4j.MessageParams
import org.eclipse.lsp4j.MessageType
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.Authorization
import playground.BuildConfig
import playground.BuildConfigDecoder
import playground.ModelReader
import playground.Runner
import playground.TextDocumentManager
import playground.TextDocumentProvider
import smithy4s.aws.AwsEnvironment
import smithy4s.aws.http4s.AwsHttp4sBackend
import smithy4s.aws.kernel.AwsRegion
import smithy4s.codegen.ModelLoader
import smithy4s.dynamic.DynamicSchemaIndex

import java.io.File
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintWriter
import java.nio.charset.Charset

object Main extends IOApp.Simple {

  private val logWriter = new PrintWriter(new File("lsp-log.txt"))

  def log(s: String): IO[Unit] = IO(logWriter.println(s))

  implicit val ioConsole: std.Console[IO] =
    new std.Console[IO] {

      def readLineWithCharset(
        charset: Charset
      ): IO[String] = IO.consoleForIO.readLineWithCharset(charset)

      def print[A](a: A)(implicit S: Show[A]): IO[Unit] = IO(logWriter.print(a.show))

      def println[A](a: A)(implicit S: Show[A]): IO[Unit] = IO(logWriter.println(a.show))

      def error[A](a: A)(implicit S: Show[A]): IO[Unit] = IO(logWriter.print("ERROR: " + a.show))

      def errorln[A](a: A)(implicit S: Show[A]): IO[Unit] = IO(
        logWriter.println("ERROR: " + a.show)
      )

    }

  def run: IO[Unit] =
    Dispatcher[IO]
      .flatMap { implicit d =>
        launch(System.in, System.out)
      }
      .use { launcher =>
        IO.interruptibleMany(launcher.startListening().get())
      } *> log("Server terminated without errors")

  def launch(
    in: InputStream,
    out: OutputStream,
  )(
    implicit d: Dispatcher[IO]
  ) = Deferred[IO, PlaygroundLanguageClient].toResource.flatMap { clientPromise =>
    makeServer(LanguageClient.suspend(clientPromise.get.map(LanguageClient.adapt(_)))).evalMap {
      server =>
        val launcher = new LSPLauncher.Builder[PlaygroundLanguageClient]()
          .setLocalService(new PlaygroundLanguageServerAdapter(server))
          .setRemoteInterface(classOf[PlaygroundLanguageClient])
          .setInput(in)
          .setOutput(out)
          .traceMessages(logWriter)
          .create();

        connect(launcher.getRemoteProxy(), clientPromise).as(launcher)
    }

  }

  private implicit val uriJsonDecoder: Decoder[Uri] = Decoder[String].emap(
    Uri.fromString(_).leftMap(_.message)
  )

  private def makeServer(
    implicit lc: LanguageClient[IO]
  ): Resource[IO, LanguageServer[IO]] = EmberClientBuilder
    .default[IO]
    .build
    .map(middleware.AuthorizationHeader[IO])
    .flatMap { client =>
      AwsEnvironment
        .default(AwsHttp4sBackend(client), AwsRegion.US_EAST_1)
        .memoize
        .flatMap { awsEnv =>
          TextDocumentManager
            .instance[IO]
            .flatMap { implicit tdm =>
              buildFile[IO].flatMap { case (buildConfig, buildFile) =>
                buildSchemaIndex(buildConfig, buildFile)
                  .flatMap { dsi =>
                    PluginResolver
                      .resolve[IO](
                        buildConfig
                          .plugins
                          .flatMap(_.smithyPlayground)
                          .flatMap(_.extensions)
                          .combineAll,
                        buildConfig.mavenRepositories.combineAll,
                      )
                      .flatTap { plugins =>
                        std.Console[IO].println("Available plugins: " + plugins.mkString(", "))
                      }
                      .map { plugins =>
                        val runner = Runner
                          .forSchemaIndex[IO](
                            dsi,
                            client,
                            LanguageClient[IO]
                              .configuration[Uri]("smithyql.http.baseUrl"),
                            awsEnv,
                            plugins = plugins,
                          )

                        LanguageServer.instance[IO](dsi, runner)
                      }
                  }
              }
            }
            .toResource
        }
    }

  def buildFile[F[_]: Sync: TextDocumentProvider]: F[(BuildConfig, Path)] = {
    val configFiles = List("build/smithy-dependencies.json", ".smithy.json", "smithy-build.json")

    fs2
      .Stream
      .emit(Path("."))
      .flatMap { folder =>
        fs2
          .Stream
          .emits(configFiles)
          .map(folder.resolve(_).absolute)
      }
      .evalMap(filePath =>
        TextDocumentProvider[F]
          .getOpt(
            filePath
              .toNioPath
              .toUri()
              .toString()
          )
          .map(_.tupleRight(filePath))
      )
      .unNone
      .head
      .compile
      .last
      .flatMap {
        _.liftTo[F](
          new Throwable("Couldn't find one of the following files: " + configFiles.mkString(", "))
        )
      }
      .flatMap { case (fileContents, filePath) =>
        BuildConfigDecoder
          .decode(fileContents.getBytes())
          .liftTo[F]
          .tupleRight(filePath)
      }
  }

  private def buildSchemaIndex(bc: BuildConfig, buildConfigPath: Path): IO[DynamicSchemaIndex] = IO
    .interruptibleMany {
      ModelLoader
        .load(
          specs =
            bc.imports
              .combineAll
              .map(
                buildConfigPath
                  .parent
                  .getOrElse(sys.error("impossible - no parent"))
                  .resolve(_)
                  .toNioPath
                  .toFile()
              )
              .toSet,
          dependencies = bc.mavenDependencies.combineAll,
          repositories = bc.mavenRepositories.combineAll,
          transformers = Nil,
          discoverModels = false,
          localJars = Nil,
        )
        ._2
    }
    .flatMap(ModelReader.buildSchemaIndex[IO])

  private def connect[C <: services.LanguageClient](
    client: C,
    clientDeff: DeferredSink[IO, C],
  ) =
    log("connecting: " + client) *>
      clientDeff.complete(client) *>
      IO(client.showMessage(new MessageParams(MessageType.Info, "hello from smithyql server"))) *>
      log("Server connected")

  private object middleware {

    def AuthorizationHeader[F[_]: Async: LanguageClient]: Client[F] => Client[F] =
      client =>
        Client[F] { request =>
          val updatedRequest =
            LanguageClient[F]
              .configuration[String]("smithyql.http.authorizationHeader")
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
