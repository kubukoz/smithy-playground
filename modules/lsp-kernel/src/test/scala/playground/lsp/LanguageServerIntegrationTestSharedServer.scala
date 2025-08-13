package playground.lsp

import cats.effect.IO
import cats.effect.Resource
import com.comcast.ip4s.*
import org.http4s.HttpRoutes
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.Server
import playground.CompilationErrorDetails.ParseError
import playground.DiagnosticSeverity
import playground.Uri
import playground.language.Command
import playground.lsp.buildinfo.BuildInfo
import playground.lsp.harness.LanguageServerIntegrationTests
import playground.lsp.harness.TestClient
import playground.smithyql.parser.ParsingFailure
import weaver.*

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

object LanguageServerIntegrationTestSharedServer
  extends IOSuite
  with LanguageServerIntegrationTests {

  override protected def checkStartupLogs: Boolean = true

  type Res = Fixture

  def sharedResource: Resource[IO, Res] = makeServer(testWorkspacesBase / "default")

  test("completions").apply { f =>
    f.server
      .completion(
        documentUri = Uri.fromPath(f.workspaceDir.toPath / "empty.smithyql"),
        position = LSPPosition(0, 0),
      )
      .map { e =>
        expect.same(
          e.map(_.item.label),
          List(
            "NextUUID",
            "GetWeather",
            "Noop",
            "CurrentTimestamp",
          ),
        )
      }
  }

  test("diagnostics") { f =>
    f.server
      .diagnostic(
        documentUri = Uri.fromPath(f.workspaceDir.toPath / "broken.smithyql")
      )
      .map { report =>
        val diagnosticItems = report

        expect(diagnosticItems.size == 1) &&
        expect.same(diagnosticItems.head.diagnostic.severity, DiagnosticSeverity.Error) &&
        expect(diagnosticItems.head.diagnostic.err.isInstanceOf[ParseError])
      }
  }

  test("document symbols") { f =>
    f.server
      .documentSymbol(
        Uri.fromPath(f.workspaceDir.toPath / "demo.smithyql")
      )
      .map { symbols =>
        expect.eql(symbols.map(_.sym.name), List("playground.std#Random", "NextUUID"))
      }
  }

  test("lens provider (run query)") { f =>
    f.server
      .codeLens(
        documentUri = Uri.fromPath(f.workspaceDir.toPath / "demo.smithyql")
      )
      .map { lenses =>
        expect.same(
          lenses.map(_.lens.command),
          List(
            Command(
              "Run SmithyQL file",
              "smithyql.runQuery",
              List(
                Uri.fromPath(f.workspaceDir.toPath / "demo.smithyql").value
              ),
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
        expect.same(
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
        expect.same(
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
        expect.eql(evs.size, 3) &&
        expect.same(evs.head, TestClient.OutputPanelShow) &&
        expect(evs(1).asInstanceOf[TestClient.OutputLog].text.contains("Calling NextUUID")) &&
        expect(evs(2).asInstanceOf[TestClient.OutputLog].text.contains("Succeeded NextUUID"))
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
        expect.eql(evs.size, 5) &&
        expect.same(evs(0), TestClient.OutputPanelShow) &&
        expect(evs(1).asInstanceOf[TestClient.OutputLog].text.contains("Calling NextUUID")) &&
        expect(evs(2).asInstanceOf[TestClient.OutputLog].text.contains("Succeeded NextUUID")) &&
        expect(
          evs(3).asInstanceOf[TestClient.OutputLog].text.contains("Calling CurrentTimestamp")
        ) &&
        expect(
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
        expect.eql(evs.size, 1) &&
        expect.same(evs(0).asInstanceOf[TestClient.MessageLog].tpe, MessageType.Error) &&
        expect(
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

        expect(hasMatchingLog)
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
        expect.eql(events.length, 4) &&
        expect.same(events(0), TestClient.OutputPanelShow) &&
        expect(
          events(1).asInstanceOf[TestClient.OutputLog].text.contains("Calling GetWeather")
        ) &&
        expect(events(2).asInstanceOf[TestClient.OutputLog].text.contains("// HTTP/1.1 GET")) &&
        expect(
          events(3)
            .asInstanceOf[TestClient.OutputLog]
            .text
            .matches("// ERROR .* Connection refused")
        )
      }
  }

}
