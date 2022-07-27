package playground.lsp

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.kernel.Deferred
import cats.effect.std.Dispatcher
import org.eclipse.lsp4j.MessageParams
import org.eclipse.lsp4j.MessageType
import org.eclipse.lsp4j.launch.LSPLauncher

import java.io.File
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintWriter
import org.eclipse.lsp4j.services
import playground.CompletionProvider
import fs2.io.file.Files
import fs2.io.file.Path
import playground.BuildConfigDecoder
import cats.implicits._
import playground.ModelReader
import smithy4s.codegen.cli.DumpModel
import smithy4s.codegen.cli.Smithy4sCommand
import playground.BuildConfig
import smithy4s.dynamic.DynamicSchemaIndex

object Main extends IOApp.Simple {

  private val logWriter = new PrintWriter(new File("lsp-log.txt"))

  def log(s: String): IO[Unit] = IO(logWriter.println(s))

  def run: IO[Unit] =
    Dispatcher[IO].use { implicit d =>
      launch(System.in, System.out)
    } *>
      log("Server terminated without errors")

  def launch(
    in: InputStream,
    out: OutputStream,
  )(
    implicit d: Dispatcher[IO]
  ) = TextDocumentManager.instance[IO].flatMap { implicit tdm =>
    // todo: workspace root
    readBuildConfig(Path("/Users/kubukoz/projects/smithy-playground-demo"))
      .flatMap(buildSchemaIndex)
      .flatMap { dsi =>
        Deferred[IO, services.LanguageClient].flatMap { clientPromise =>
          implicit val client: LanguageClient[IO] = LanguageClient.adapt(clientPromise.get)

          val server = new PlaygroundLanguageServerAdapter(LanguageServer.instance[IO](dsi))

          val launcher = new LSPLauncher.Builder[services.LanguageClient]()
            .setLocalService(server)
            .setRemoteInterface(classOf[services.LanguageClient])
            .setInput(in)
            .setOutput(out)
            .traceMessages(logWriter)
            .create();

          connect(launcher.getRemoteProxy(), clientPromise) *>
            IO.interruptibleMany(launcher.startListening().get())
        }
      }
  }

  private def readBuildConfig(ctx: Path) = Files[IO]
    .readAll(ctx / "smithy-build.json")
    .compile
    .toVector
    .map(_.toArray)
    .flatMap {
      BuildConfigDecoder.decode(_).liftTo[IO]
    }

  private def buildSchemaIndex(bc: BuildConfig): IO[DynamicSchemaIndex] = IO
    .interruptibleMany {
      DumpModel.run(
        Smithy4sCommand.DumpModelArgs(
          specs = bc.imports.combineAll.map(os.Path(_)),
          repositories = bc.mavenRepositories.combineAll,
          dependencies = bc.mavenDependencies.combineAll,
          transformers = Nil,
          localJars = Nil,
        )
      )
    }
    .flatMap { modelText =>
      ModelReader
        .modelParser(modelText)
        .liftTo[IO]
        .map(ModelReader.buildSchemaIndex(_))
    }

  private def connect(
    client: services.LanguageClient,
    clientDeff: Deferred[IO, services.LanguageClient],
  ) =
    log("connecting: " + client) *>
      clientDeff.complete(client) *>
      IO(client.showMessage(new MessageParams(MessageType.Info, "hello from smithyql server"))) *>
      log("Server connected")

}
