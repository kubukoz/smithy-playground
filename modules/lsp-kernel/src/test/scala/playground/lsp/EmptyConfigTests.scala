package playground.lsp

import cats.effect.IO
import cats.effect.kernel.Resource
import playground.lsp.harness.LanguageServerIntegrationTests
import playground.lsp.harness.TestClient.Event
import weaver.*

object EmptyConfigTests extends IOSuite with LanguageServerIntegrationTests {

  type Res = Fixture

  def sharedResource: Resource[IO, Res] = makeServer(testWorkspacesBase / "default")

  test("server init produces logs consistent with the workspace folder") { f =>
    f.client
      .scoped {
        f.server.didChangeWatchedFiles *>
          f.client.getEvents
      }
      .map { events =>
        expect.same(
          events,
          List(
            Event.MessageLog(MessageType.Info, "No change detected, not rebuilding server")
          ),
        )
      }
  }

}
