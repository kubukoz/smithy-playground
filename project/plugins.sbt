ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always

addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.11")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.4.3")

addSbtPlugin("com.disneystreaming.smithy4s" % "smithy4s-sbt-codegen" % "0.18.0-717-e636191")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.1")

addDependencyTreePlugin
