package playground.lsp

import cats.effect.IO
import playground.PlaygroundConfig
import weaver.*

object PluginResolverTests extends SimpleIOSuite {
  test("Empty plugin resolver finds no plugins") {
    PluginResolver
      .instance[IO]
      .resolve(PlaygroundConfig.empty)
      .map(expect.same(_, Nil))
  }

  test("Plugin resolver doesn't check maven dependencies") {
    PluginResolver
      .instance[IO]
      .resolve(
        PlaygroundConfig
          .empty
          .copy(
            dependencies = List(
              "com.kubukoz.playground::plugin-sample:latest.integration"
            )
          )
      )
      .map(expect.same(_, Nil))
  }

  test("Plugin resolver with a sample plugin artifact finds it") {
    PluginResolver
      .instance[IO]
      .resolve(
        PlaygroundConfig
          .empty
          .copy(
            extensions = List(
              "com.kubukoz.playground::plugin-sample:latest.integration"
            )
          )
      )
      .map(_.map(_.getClass().getName()))
      .map(expect.same(_, List("playground.sample.SamplePlaygroundPlugin")))
  }
}
