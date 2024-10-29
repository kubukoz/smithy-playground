package playground.lsp.harness

import cats.effect.IO
import cats.effect.IOLocal
import cats.effect.Resource
import cats.syntax.all.*
import org.eclipse.lsp4j.InitializeParams
import org.eclipse.lsp4j.MessageType
import org.eclipse.lsp4j.WorkspaceFolder
import playground.language.Uri
import playground.lsp.LanguageServer
import playground.lsp.MainServer
import playground.lsp.buildinfo.BuildInfo
import weaver.SourceLocation

import scala.jdk.CollectionConverters.*
import scala.util.chaining.*

trait LanguageServerIntegrationTests {

  protected def checkStartupLogs: Boolean = false

  case class Fixture(
    server: LanguageServer[IO],
    client: TestClient[IO],
    workspaceDir: Uri,
  )

  def initParams(
    workspaceDir: Uri
  ): InitializeParams = new InitializeParams().tap(
    _.setWorkspaceFolders(
      List(
        new WorkspaceFolder(workspaceDir.value, "test-workspace")
      ).asJava
    )
  )

  def makeServer(
    workspaceDir: Uri
  ): Resource[IO, Fixture] = TestClient.forIO.toResource.flatMap { implicit client =>
    MainServer
      .makeServer[IO]
      .evalMap { server =>
        val result = Fixture(
          server = server,
          client = client,
          workspaceDir = workspaceDir,
        )

        server.initialize(initParams(workspaceDir)) *>
          assertStartupEvents(client)
            .as(result)
      }
  }

  /*
   * Note - this should've been a test (and used to).
   * For some reason, it no longer worked because logs produced by the initialize call were suddenly being lost after resource initialization.
   * The underlying cause for event loss was broken IOLocal propagation in the event of lifting resources to fs2 streams
   * (resulting in the resource being acquired step-by-step with multiple fibers).
   *
   * If the underlying issue is solved, this can again be turned into a test.
   */
  private def assertStartupEvents(
    client: TestClient[IO]
  ): IO[Unit] = client
    .getEvents
    .flatMap { events =>
      val initLogs = List(
        TestClient
          .MessageLog(
            MessageType.Info,
            s"Hello from Smithy Playground v${BuildInfo.version}! Loading project...",
          ),
        TestClient.MessageLog(
          MessageType.Info,
          "Loaded Smithy Playground server with 2 source entries, 0 imports, 2 dependencies and 0 plugins",
        ),
      )
      IO {
        require(
          events.size == initLogs.size,
          s"Events emitted at startup should've had size ${initLogs.size}, it was " + events.size + " instead",
        )
        require(
          events == initLogs,
          "Events were not as expected, got: " + events,
        )
      }
    }
    .whenA(checkStartupLogs)

  def testWorkspacesBase: Uri = Uri.fromUriString(
    getClass
      .getResource("/test-workspaces")
      .toURI()
      .toString()
  )

}
