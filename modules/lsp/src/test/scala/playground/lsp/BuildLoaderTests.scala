package playground.lsp

import weaver._
import cats.effect.IO
import playground.language.TextDocumentProvider
import playground.BuildConfig
import fs2.io.file.Path

object BuildLoaderTests extends SimpleIOSuite {
  // won't actually be used, but still TODO: use a stub/fake
  implicit val tdp: TextDocumentProvider[IO] = null

  val loader = BuildLoader.instance[IO]

  test("build loader can see stdlib") {
    val result = loader
      .buildSchemaIndex(BuildLoader.Loaded(BuildConfig(), Path(".")))
      .map(_.allServices)

    result.map { services =>
      assert.eql(services.map(_.service.id.name), List("Random", "Clock"))
    }
  }

  // todo: tests with some specs?
}
