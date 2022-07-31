package playground.lsp

import cats.Defer
import cats.Show
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.effect.kernel.Ref
import cats.effect.kernel.Resource
import cats.effect.kernel.Sync
import cats.effect.std
import cats.effect.std.Dispatcher
import cats.implicits._
import fs2.io.file.Path
import io.circe.Decoder
import org.eclipse.lsp4j.launch.LSPLauncher
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
import playground.lsp.buildinfo.BuildInfo
import smithy4s.aws.AwsEnvironment
import smithy4s.aws.http4s.AwsHttp4sBackend
import smithy4s.aws.kernel.AwsRegion
import smithy4s.codegen.ModelLoader
import smithy4s.dynamic.DynamicSchemaIndex

import java.io.File
import java.io.FileOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import java.io.PrintWriter
import java.nio.charset.Charset

object Main extends IOApp.Simple {

  private val logOut = new PrintStream(new FileOutputStream(new File("smithyql-log.txt")))
  private val logWriter = new PrintWriter(logOut)

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

  def log[F[_]: std.Console](s: String): F[Unit] = std.Console[F].println(s)

  def run: IO[Unit] =
    Dispatcher[IO]
      .flatMap { implicit d =>
        std.Supervisor[IO].flatMap { implicit sup =>
          val stdin = System.in
          val stdout = System.out

          IO(System.setOut(logOut)).toResource *>
            launch(stdin, stdout)
        }
      }
      .use { launcher =>
        IO.interruptibleMany(launcher.startListening().get())
      } *> log("Server terminated without errors")

  def launch(
    in: InputStream,
    out: OutputStream,
  )(
    implicit d: Dispatcher[IO],
    sup: std.Supervisor[IO],
  ) = IO.ref((LanguageServer.notAvailable[IO], none[BuildConfig])).toResource.flatMap { serverRef =>
    val server = LanguageServer.defer(serverRef.get.map(_._1))

    val launcher = new LSPLauncher.Builder[PlaygroundLanguageClient]()
      .setLocalService(new PlaygroundLanguageServerAdapter(server))
      .setRemoteInterface(classOf[PlaygroundLanguageClient])
      .setInput(in)
      .setOutput(out)
      .traceMessages(logWriter)
      .create();

    implicit val client: LanguageClient[IO] = LanguageClient.adapt[IO](launcher.getRemoteProxy())

    connect(serverRef).as(launcher)
  }

  private implicit val uriJsonDecoder: Decoder[Uri] = Decoder[String].emap(
    Uri.fromString(_).leftMap(_.message)
  )

  /** Produces a server using the global resources. Anything instantiated here should be considered
    * ephemeral and may be rebuilt on every change to the build files.
    */
  private def makeServerInstance[F[_]: LanguageClient: TextDocumentManager: Async: std.Console](
    buildConfig: BuildConfig,
    buildFilePath: Path,
    client: Client[F],
    awsEnv: Resource[F, AwsEnvironment[F]],
    reload: F[Unit],
  ): F[LanguageServer[F]] = buildSchemaIndex(buildConfig, buildFilePath)
    .flatMap { dsi =>
      PluginResolver
        .resolveFromConfig[F](buildConfig)
        .map { plugins =>
          val runner = Runner
            .forSchemaIndex[F](
              dsi,
              client,
              LanguageClient[F]
                .configuration[Uri]("smithyql.http.baseUrl"),
              awsEnv,
              plugins = plugins,
            )

          LanguageServer.instance[F](dsi, runner, reload)
        }
    }

  private def makeServer[F[_]: Async: std.Console](
    serverRef: Ref[F, (LanguageServer[F], Option[BuildConfig])]
  )(
    implicit lc: LanguageClient[F],
    sup: std.Supervisor[F],
  ): Resource[F, Unit] = EmberClientBuilder
    .default[F]
    .build
    .map(middleware.AuthorizationHeader[F])
    .flatMap { client =>
      AwsEnvironment
        .default(AwsHttp4sBackend(client), AwsRegion.US_EAST_1)
        .memoize
        .flatMap { awsEnv =>
          TextDocumentManager
            .instance[F]
            .flatMap { implicit tdm =>
              def mkServer(bc: BuildConfig, p: Path, reload: F[Unit])
                : F[Unit] = makeServerInstance(bc, p, client, awsEnv, reload)
                .tupleRight(bc.some)
                .flatMap(serverRef.set)

              buildFile[F]
                .flatMap { case (initialBuildConfig, buildFilePath) =>
                  val reload = Defer[F].fix[Unit] { self =>
                    buildFile[F].flatMap { case (bc, path) =>
                      val completeAsync =
                        LanguageClient[F].refreshDiagnostics *>
                          LanguageClient[F].refreshCodeLenses *> LanguageClient[F].showInfoMessage(
                            s"Reloaded Smithy Playground server with " +
                              s"${bc.imports.combineAll.size} imports, " +
                              s"${bc.mavenDependencies.combineAll.size} dependencies and " +
                              s"${bc.plugins.flatMap(_.smithyPlayground).flatMap(_.extensions).size} plugins"
                          )

                      serverRef.get.map(_._2).flatMap { previousBuildConfig =>
                        if (previousBuildConfig.contains(bc))
                          LanguageClient[F].showInfoMessage(
                            "No change detected, not rebuilding server"
                          )
                        else
                          LanguageClient[F].showInfoMessage(
                            "Detected changes, will try to rebuild server..."
                          ) *>
                            mkServer(bc, path, self).onError { case e =>
                              LanguageClient[F].showErrorMessage(
                                "Couldn't reload server: " + e.getMessage
                              )
                            } *>
                            // Can't make (and wait for) client requests while handling a client request (file change)
                            completeAsync.supervise(sup).void
                      }
                    }
                  }

                  mkServer(initialBuildConfig, buildFilePath, reload)
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

  private def buildSchemaIndex[F[_]: Sync](
    bc: BuildConfig,
    buildConfigPath: Path,
  ): F[DynamicSchemaIndex] = Sync[F]
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
    .flatMap(ModelReader.buildSchemaIndex[F])

  private def connect[F[_]: Async: LanguageClient: std.Console: std.Supervisor](
    serverRef: Ref[F, (LanguageServer[F], Option[BuildConfig])]
  ): Resource[F, Unit] =
    log[F]("connecting").toResource *>
      makeServer(serverRef) *>
      LanguageClient[F]
        .showInfoMessage(s"Hello from Smithy Playground v${BuildInfo.version}")
        .toResource *>
      log[F]("Server connected").toResource

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
