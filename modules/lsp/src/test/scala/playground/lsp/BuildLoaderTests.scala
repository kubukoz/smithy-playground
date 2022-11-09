package playground.lsp

import cats.effect.IO
import fs2.io.file.Path
import playground.BuildConfig
import playground.language.TextDocumentProvider
import weaver._

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

  test("build loader can see external services") {
    val result = loader
      .buildSchemaIndex(
        BuildLoader.Loaded(
          BuildConfig(mavenDependencies =
            Some(
              List(
                // todo
              )
            )
          ),
          Path("."),
        )
      )
      .map(_.allServices)

    result.map { services =>
      assert.eql(services.map(_.service.id.name), List("Random", "Clock", "SthElse"))
    }
  }

}
