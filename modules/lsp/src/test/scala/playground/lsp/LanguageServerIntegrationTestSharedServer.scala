package playground.lsp

import cats.effect.IO
import cats.effect.Resource
import com.comcast.ip4s._
import org.eclipse.lsp4j.CodeLensParams
import org.eclipse.lsp4j.Command
import org.eclipse.lsp4j.CompletionParams
import org.eclipse.lsp4j.DiagnosticSeverity
import org.eclipse.lsp4j.DocumentDiagnosticParams
import org.eclipse.lsp4j.DocumentSymbolParams
import org.eclipse.lsp4j.MessageType
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.TextDocumentIdentifier
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server
import playground.language.Uri
import playground.lsp.buildinfo.BuildInfo
import smithy4s.http4s.SimpleRestJsonBuilder
import weather.GetWeatherOutput
import weather.GoodWeather
import weather.Weather
import weather.WeatherService
import weaver._

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

object LanguageServerIntegrationTestSharedServer
  extends IOSuite
  with LanguageServerIntegrationTests {

  type Res = Fixture

  def sharedResource: Resource[IO, Res] = makeServer(resourceUri("/test-workspace"))

  test("server init produces logs consistent with the workspace folder") { f =>
    val initLogs = List(
      TestClient
        .MessageLog(
          MessageType.Info,
          s"Hello from Smithy Playground v${BuildInfo.version}",
        ),
      TestClient.MessageLog(
        MessageType.Info,
        "Loaded Smithy Playground server with 1 imports, 0 dependencies and 0 plugins",
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
              "Run query",
              "smithyql.runQuery",
              List(
                Uri.fromPath(f.workspaceDir.toPath / "demo.smithyql").value: Object
              ).asJava,
            )
          ),
        )
      }
  }

  test("smithyql/runQuery (in memory)") { f =>
    f.client
      .scoped {
        f.server
          .runQuery(
            RunQueryParams(
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

  val fakeServer: Resource[IO, Server] =
    EmberServerBuilder
      .default[IO]
      // deliberately not exposing on 0.0.0.0
      // as there's no need
      .withHost(host"localhost")
      .withPort(port"0")
      .withShutdownTimeout(1.second)
      .withHttpApp(
        SimpleRestJsonBuilder
          .routes(
            new WeatherService[IO] {

              def getWeather(city: String): IO[GetWeatherOutput] = IO.pure(
                GetWeatherOutput(Weather.GoodCase(GoodWeather(reallyGood = Some(true))))
              )

            }
          )
          .make
          .toTry
          .get
          .orNotFound
      )
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
            .runQuery(
              RunQueryParams(
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

}