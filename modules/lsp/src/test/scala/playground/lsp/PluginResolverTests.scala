package playground.lsp

import cats.effect.IO
import weaver._

object PluginResolverTests extends SimpleIOSuite {
  test("Empty plugin resolver finds no plugins") {
    PluginResolver
      .instance[IO]
      .resolve(artifacts = Nil, repositories = Nil)
      .map(assert.same(_, Nil))
  }

  test("Plugin resolver with a sample plugin artifact finds it") {
    PluginResolver
      .instance[IO]
      .resolve(
        artifacts = List(
          "com.kubukoz.playground::plugin-sample:latest.integration"
        ),
        repositories = Nil,
      )
      .map(_.map(_.getClass().getName()))
      .map(assert.same(_, List("playground.sample.SamplePlaygroundPlugin")))
  }
}
