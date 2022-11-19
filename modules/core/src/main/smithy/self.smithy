$version: "2"

namespace playground

structure BuildConfig {
  mavenDependencies: Strings = [],
  mavenRepositories: Strings = [],
  imports: Strings = [],
  plugins: Plugins
}

structure Plugins {
  @jsonName("smithy-playground")
  smithyPlayground: SmithyPlaygroundPluginConfig
}

structure SmithyPlaygroundPluginConfig {
  extensions: Strings = []
}

list Strings { member: String }
