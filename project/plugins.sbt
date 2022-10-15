addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.10")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.4.1")

// todo: 0.16.4
addSbtPlugin("com.disneystreaming.smithy4s" % "smithy4s-sbt-codegen" % "dev-SNAPSHOT")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.1")

addDependencyTreePlugin
