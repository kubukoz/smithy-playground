package playground.lsp

import cats.effect.IO
import cats.effect.Resource
import com.comcast.ip4s.*
import org.eclipse.lsp4j.CodeLensParams
import org.eclipse.lsp4j.Command
import org.eclipse.lsp4j.CompletionParams
import org.eclipse.lsp4j.DiagnosticSeverity
import org.eclipse.lsp4j.DocumentDiagnosticParams
import org.eclipse.lsp4j.DocumentSymbolParams
import org.eclipse.lsp4j.MessageType
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.TextDocumentIdentifier
import org.http4s.HttpRoutes
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.Server
import playground.language.Uri
import playground.lsp.buildinfo.BuildInfo
import playground.lsp.harness.LanguageServerIntegrationTests
import playground.lsp.harness.TestClient
import weaver.*

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

object LanguageServerIntegrationTestSharedServer
  extends IOSuite
  with LanguageServerIntegrationTests {

  type Res = Fixture

  def sharedResource: Resource[IO, Res] = makeServer(testWorkspacesBase / "default")

  test("server init produces logs consistent with the workspace folder") { f =>
    val initLogs = List(
      TestClient
        .MessageLog(
          MessageType.Info,
          s"Hello from Smithy Playground v${BuildInfo.version}",
        ),
      TestClient.MessageLog(
        MessageType.Info,
        "Loaded Smithy Playground server with 2 sources, 0 imports, 2 dependencies and 0 plugins",
      ),
    )

    // logs produced during an implicit initialization in the resource setup
    f.client.getEvents.map { events =>
      assert.same(
        events,
        initLogs,
      )
    }
  }

  test("completions").apply { f =>
    f.server
      .completion(
        new CompletionParams(
          new TextDocumentIdentifier(
            Uri.fromPath(f.workspaceDir.toPath / "empty.smithyql").value
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
              "GetWeather",
              "Noop",
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
            (f.workspaceDir.toPath / "broken.smithyql").toNioPath.toUri().toString()
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
            Uri.fromPath(f.workspaceDir.toPath / "demo.smithyql").value
          )
        )
      )
      .map { symbols =>
        assert.eql(symbols.map(_.getName()), List("playground.std#Random", "NextUUID"))
      }
  }

  test("lens provider (run query)") { f =>
    f.server
      .codeLens(
        new CodeLensParams(
          new TextDocumentIdentifier(
            Uri.fromPath(f.workspaceDir.toPath / "demo.smithyql").value
          )
        )
      )
      .map { lenses =>
        assert.same(
          lenses.map(_.getCommand()),
          List(
            new Command(
              "Run SmithyQL file",
              "smithyql.runQuery",
              List(
                Uri.fromPath(f.workspaceDir.toPath / "demo.smithyql").value: Object
              ).asJava,
            )
          ),
        )
      }
  }

  test("smithyql/runQuery (empty file)") { f =>
    f.client
      .scoped {
        f.server
          .runFile(
            RunFileParams(
              Uri.fromPath(f.workspaceDir.toPath / "empty.smithyql")
            )
          ) *> f.client.getEvents
      }
      .map { evs =>
        assert.same(
          evs,
          List(TestClient.MessageLog(MessageType.Warning, "No operations to run in file")),
        )
      }
  }

  test("smithyql/runQuery (output panel)") { f =>
    f.client
      .scoped {
        f.server
          .runFile(
            RunFileParams(
              Uri.fromUriString("output:anything_here")
            )
          ) *> f.client.getEvents
      }
      .map { evs =>
        assert.same(
          evs,
          Nil,
        )
      }
  }

  test("smithyql/runQuery (in memory)") { f =>
    f.client
      .scoped {
        f.server
          .runFile(
            RunFileParams(
              Uri.fromPath(f.workspaceDir.toPath / "demo.smithyql")
            )
          ) *> f.client.getEvents
      }
      .map { evs =>
        assert.eql(evs.size, 3) &&
        assert.same(evs.head, TestClient.OutputPanelShow) &&
        assert(evs(1).asInstanceOf[TestClient.OutputLog].text.contains("Calling NextUUID")) &&
        assert(evs(2).asInstanceOf[TestClient.OutputLog].text.contains("Succeeded NextUUID"))
      }
  }

  // this and some others could become component tests for the runner...
  // ...if we ever get any.
  test("smithyql/runQuery (in memory, multiple queries)") { f =>
    f.client
      .scoped {
        f.server
          .runFile(
            RunFileParams(
              Uri.fromPath(f.workspaceDir.toPath / "multi-query.smithyql")
            )
          ) *> f.client.getEvents
      }
      .map { evs =>
        assert.eql(evs.size, 5) &&
        assert.same(evs(0), TestClient.OutputPanelShow) &&
        assert(evs(1).asInstanceOf[TestClient.OutputLog].text.contains("Calling NextUUID")) &&
        assert(evs(2).asInstanceOf[TestClient.OutputLog].text.contains("Succeeded NextUUID")) &&
        assert(
          evs(3).asInstanceOf[TestClient.OutputLog].text.contains("Calling CurrentTimestamp")
        ) &&
        assert(
          evs(4).asInstanceOf[TestClient.OutputLog].text.contains("Succeeded CurrentTimestamp")
        )
      }
  }

  test("smithyql/runQuery (in memory, multiple queries, multiple without runner)") { f =>
    f.client
      .scoped {
        f.server
          .runFile(
            RunFileParams(
              Uri.fromPath(f.workspaceDir.toPath / "multi-query-partial-runner.smithyql")
            )
          ) *> f.client.getEvents
      }
      .map { evs =>
        assert.eql(evs.size, 1) &&
        assert.same(evs(0).asInstanceOf[TestClient.MessageLog].tpe, MessageType.Error) &&
        assert(
          evs(0)
            .asInstanceOf[TestClient.MessageLog]
            .msg
            .startsWith("At least 1 service in the file uses an unsupported protocol.")
        )
      }
  }

  val fakeServer: Resource[IO, Server] =
    EmberServerBuilder
      .default[IO]
      // deliberately not exposing on 0.0.0.0
      // as there's no need
      .withHost(host"localhost")
      // random port
      .withPort(port"0")
      .withShutdownTimeout(Duration.Zero)
      .withHttpApp {
        import org.http4s.dsl.io.*
        HttpRoutes
          .of[IO] { case GET -> Root / "weather" / _ =>
            Ok(
              s"""{
                 |  "weather": {
                 |    "good": {
                 |      "reallyGood": true
                 |    }
                 |  }
                 |}""".stripMargin
            )
          }
          .orNotFound
      }
      .build

  test("HTTP calls using configured base uri") { f =>
    fakeServer
      .use { httpServer =>
        val env = f
          .client
          .scoped
          .compose(
            f.client.withConfiguration(ConfigurationValue.baseUri(httpServer.baseUri))
          )

        env {
          f.server
            .runFile(
              RunFileParams(
                Uri.fromPath(f.workspaceDir.toPath / "http-demo.smithyql")
              )
            ) *> f.client.getEvents
        }
      }
      .map { events =>
        val hasMatchingLog = events
          .collect { case l: TestClient.OutputLog => l }
          .exists(_.text.contains("Succeeded GetWeather"))

        assert(hasMatchingLog)
      }
  }

  test("HTTP calls: connection failure cancels the request") { f =>
    val env = f
      .client
      .scoped
      .compose(f.client.withConfiguration(ConfigurationValue.baseUri(uri"http://localhost:80")))

    env {
      f.server
        .runFile(
          RunFileParams(
            Uri.fromPath(f.workspaceDir.toPath / "multi-query-one-failing.smithyql")
          )
        ) *> f.client.getEvents
    }
      .map { events =>
        assert.eql(events.length, 4) &&
        assert.same(events(0), TestClient.OutputPanelShow) &&
        assert(events(1).asInstanceOf[TestClient.OutputLog].text.contains("Calling GetWeather")) &&
        assert(events(2).asInstanceOf[TestClient.OutputLog].text.contains("// HTTP/1.1 GET")) &&
        assert(
          events(3)
            .asInstanceOf[TestClient.OutputLog]
            .text
            .matches("// ERROR .* Connection refused")
        )
      }
  }

}
