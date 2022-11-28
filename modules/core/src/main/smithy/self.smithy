$version: "2"

namespace playground

structure BuildConfig {
  mavenDependencies: Strings = [],
  mavenRepositories: Strings = [],
  maven: MavenConfig,
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

structure MavenConfig {
  dependencies: Strings = [],
  repositories: Repositories = []
}

list Repositories { member: Repository }

structure Repository {
  @required
  url: String
}
