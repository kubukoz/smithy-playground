package playground.lsp

import cats.effect.IO
import cats.effect.Resource
import cats.effect.implicits._
import org.eclipse.lsp4j.CompletionParams
import org.eclipse.lsp4j.DiagnosticSeverity
import org.eclipse.lsp4j.DocumentDiagnosticParams
import org.eclipse.lsp4j.InitializeParams
import org.eclipse.lsp4j.MessageType
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.TextDocumentIdentifier
import org.eclipse.lsp4j.WorkspaceFolder
import playground.language.Uri
import playground.lsp.buildinfo.BuildInfo
import weaver._

import scala.jdk.CollectionConverters._
import scala.util.chaining._
import org.eclipse.lsp4j.DocumentSymbolParams
import org.eclipse.lsp4j.SymbolKind

object LanguageServerIntegrationTest extends IOSuite {

  case class Fixture(
    server: LanguageServer[IO],
    client: TestClient[IO],
    workspaceDir: Uri,
  )

  type Res = Fixture

  def initParams(workspaceDir: Uri) = new InitializeParams().tap(
    _.setWorkspaceFolders(
      List(
        new WorkspaceFolder(workspaceDir.value)
      ).asJava
    )
  )

  def makeServer(
    workspaceDir: Uri
  ): Resource[IO, Fixture] = TestClient.forIO.toResource.flatMap { implicit client =>
    Main
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

  def sharedResource: Resource[IO, Res] = makeServer(resourceUri("/test-workspace"))

  def resourceUri(resourcePath: String): Uri = Uri(
    Option(getClass.getResource(resourcePath))
      .getOrElse(sys.error(s"oops, not found: resource $name"))
      .toURI()
      .toString()
  )

  test("server init produces logs consistent with the workspace folder") { f =>
    val initLogs = List(
      TestClient
        .MessageLog(
          MessageType.Info,
          s"Hello from Smithy Playground v${BuildInfo.version}",
        ),
      TestClient.MessageLog(
        MessageType.Info,
        "Loaded Smithy Playground server with 0 imports, 0 dependencies and 0 plugins",
      ),
    )

    // logs produced during an implicit initialization in the resource setup
    f.client.getLogs.map { logs =>
      assert.same(
        logs,
        initLogs,
      )
    }
  }

  test("completions") { f =>
    f.server
      .completion(
        new CompletionParams(
          new TextDocumentIdentifier(
            (f.workspaceDir.toPath / "empty.smithyql").toNioPath.toUri().toString()
          ),
          new Position(0, 0),
        )
      )
      .map {
        case Left(e) =>
          assert.same(
            e.map(_.getLabel()),
            List(
              "NextUUID",
              "CurrentTimestamp",
            ),
          )
        case Right(v) => failure(s"Right was found, but Left was expected: $v")
      }
  }

  test("diagnostics") { f =>
    f.server
      .diagnostic(
        new DocumentDiagnosticParams(
          new TextDocumentIdentifier(
            (f.workspaceDir.toPath / "empty.smithyql").toNioPath.toUri().toString()
          )
        )
      )
      .map { report =>
        val diagnosticItems =
          report.getRelatedFullDocumentDiagnosticReport().getItems().asScala.toList

        assert(diagnosticItems.size == 1) &&
        assert(diagnosticItems.head.getSeverity() == DiagnosticSeverity.Error) &&
        assert(diagnosticItems.head.getMessage.contains("Parsing failure"))
      }
  }

  test("document symbols") { f =>
    f.server
      .documentSymbol(
        new DocumentSymbolParams(
          new TextDocumentIdentifier(
            (f.workspaceDir.toPath / "demo.smithyql").toNioPath.toUri().toString()
          )
        )
      )
      .map { symbols =>
        assert.eql(symbols.map(_.getName()), List("playground.std#Random", "NextUUID"))
      }
  }
}
