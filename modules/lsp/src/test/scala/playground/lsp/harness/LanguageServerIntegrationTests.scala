package playground.lsp.harness

import cats.effect.IO
import cats.effect.Resource
import cats.effect.implicits._
import org.eclipse.lsp4j.InitializeParams
import org.eclipse.lsp4j.WorkspaceFolder
import playground.language.Uri
import playground.lsp.LanguageServer
import playground.lsp.MainServer

import scala.jdk.CollectionConverters._
import scala.util.chaining._

trait LanguageServerIntegrationTests {

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
        new WorkspaceFolder(workspaceDir.value)
      ).asJava
    )
  )

  def makeServer(
    workspaceDir: Uri
  ): Resource[IO, Fixture] = TestClient.forIO.toResource.flatMap { implicit client =>
    MainServer
      .makeServer[IO]
      .evalTap(_.initialize(initParams(workspaceDir)))
      .map { server =>
        Fixture(
          server = server,
          client = client,
          workspaceDir = workspaceDir,
        )
      }
  }

  def testWorkspacesBase: Uri = Uri.fromUriString(
    getClass
      .getResource("/test-workspaces")
      .toURI()
      .toString()
  )

}
