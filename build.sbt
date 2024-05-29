inThisBuild(
  List(
    organization := "com.kubukoz",
    homepage := Some(url("https://github.com/kubukoz/smithy-playground")),
    licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "kubukoz",
        "Jakub Kozłowski",
        "kubukoz@gmail.com",
        url("https://kubukoz.com"),
      )
    ),
  )
)

import scala.sys.process.*

def crossPlugin(
  x: sbt.librarymanagement.ModuleID
) = compilerPlugin(x.cross(CrossVersion.full))

val compilerPlugins =
  libraryDependencies ++= List(
    crossPlugin("org.polyvariant" % "better-tostring" % "0.3.17")
  ) ++ (if (scalaVersion.value.startsWith("3"))
          Nil
        else
          List(
            crossPlugin("org.typelevel" % "kind-projector" % "0.13.3")
          ))

ThisBuild / versionScheme := Some("early-semver")

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := "2.13.13"
ThisBuild / crossScalaVersions := Seq("2.13.13")

// For coursier's "latest.integration"
ThisBuild / dynverSeparator := "-"

val commonSettings = Seq(
  organization := "com.kubukoz.playground",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.12.0",
    "org.typelevel" %% "cats-mtl" % "1.4.0",
    "com.disneystreaming" %% "weaver-cats" % "0.8.4" % Test,
    "com.disneystreaming" %% "weaver-discipline" % "0.8.4" % Test,
    "com.disneystreaming" %% "weaver-scalacheck" % "0.8.4" % Test,
    "com.softwaremill.diffx" %% "diffx-core" % "0.9.0" % Test,
    "com.softwaremill.diffx" %% "diffx-cats" % "0.9.0" % Test,
  ),
  compilerPlugins,
  scalacOptions -= "-Xfatal-warnings",
  scalacOptions -= "-Vtype-diffs",
  scalacOptions += "-Wnonunit-statement",
  scalacOptions ++= Seq("-Xsource:3.0"),
  Test / scalacOptions += "-Wconf:cat=deprecation:silent,msg=Specify both message and version:silent",
  scalacOptions ++= Seq("-release", "11"),
  mimaFailOnNoPrevious := false,
)

def module(
  name: String
) = Project(name, file("modules") / name)
  .settings(
    commonSettings
  )

// Plugin interface. Keeps binary compatibility guarantees (mostly tied to smithy4s's bincompat).
lazy val pluginCore = module("plugin-core").settings(
  libraryDependencies ++= Seq(
    "com.disneystreaming.smithy4s" %% "smithy4s-http4s" % smithy4sVersion.value
  ),
  // mimaPreviousArtifacts := Set(organization.value %% name.value % "0.3.0"),
  mimaPreviousArtifacts := Set.empty,
)

lazy val pluginSample = module("plugin-sample")
  .dependsOn(pluginCore)
  .settings(
    // used for tests only
    mimaPreviousArtifacts := Set.empty
  )

// AST of SmithyQL language (plus DSL and minor utilities for building these)
lazy val ast = module("ast")

// Source code model (comments, locations, etc.)
lazy val source = module("source")
  .dependsOn(ast)

// Parser interface / implementation
lazy val parser = module("parser")
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "1.0.0",
      "io.circe" %% "circe-generic" % "0.14.7" % Test,
      "io.circe" %% "circe-parser" % "0.14.7" % Test,
      "co.fs2" %% "fs2-io" % "3.10.2" % Test,
    )
  )
  .dependsOn(
    ast % "test->test;compile->compile",
    source % "test->test;compile->compile",
  )

// Formatter for the SmithyQL language constructs
lazy val formatter = module("formatter")
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "paiges-cats" % "0.4.3"
    )
  )
  .dependsOn(
    ast,
    source,
    parser % "test->test",
  )

// Most of the core functionality of SmithyQL (compilation, analysis, evaluation)
// also: SmithyQL standard library
lazy val core = module("core")
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.5.4",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.28.5",
      "com.disneystreaming.smithy4s" %% "smithy4s-dynamic" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" %% "smithy4s-http4s" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" %% "smithy4s-aws-http4s" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" % "smithy4s-protocol" % smithy4sVersion.value % Test,
      "com.disneystreaming.alloy" % "alloy-core" % "0.3.8" % Test,
      "software.amazon.smithy" % "smithy-aws-traits" % "1.49.0" % Test,
    ),
    Smithy4sCodegenPlugin.defaultSettings(Test),
  )
  .enablePlugins(Smithy4sCodegenPlugin)
  .dependsOn(
    pluginCore,
    ast,
    source % "test->test;compile->compile",
    parser % "test->compile;test->test",
    formatter,
  )

// LSP-like interfaces like CodeLensProvider, which are later adapted into actual lsp
lazy val languageSupport = module("language-support")
  .dependsOn(core % "test->test;compile->compile", parser)

// Adapters for language services to LSP, actual LSP server binding, entrypoint
lazy val lsp = module("lsp")
  .settings(
    libraryDependencies ++= Seq(
      "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.23.1",
      "io.circe" %% "circe-core" % "0.14.7",
      "org.http4s" %% "http4s-ember-client" % "0.23.27",
      "org.http4s" %% "http4s-ember-server" % "0.23.27" % Test,
      "io.get-coursier" %% "coursier" % "2.1.10",
      "org.typelevel" %% "cats-tagless-core" % "0.16.1",
    ),
    buildInfoPackage := "playground.lsp.buildinfo",
    buildInfoKeys ++= Seq(version, scalaBinaryVersion),
    (Test / test) := {
      (pluginCore / publishLocal).value
      (pluginSample / publishLocal).value

      (Test / test).value
    },
  )
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(languageSupport)

lazy val e2e = module("e2e")
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys ++=
      Seq[BuildInfoKey.Entry[_]]( // do you know how to simplify this? let me know please!
        Def
          .task((lsp / Compile / fullClasspath).value.map(_.data).map(_.toString))
          .taskValue
          .named("lspClassPath"),
        Def
          .task(
            (lsp / Compile / mainClass).value.getOrElse(sys.error("didn't find main class in lsp"))
          )
          .taskValue
          .named("lspMainClass"),
      ),
    publish / skip := true,
  )
  .dependsOn(lsp)

val writeVersion = taskKey[Unit]("Writes the current version to the `.version` file")

lazy val root = project
  .in(file("."))
  .settings(
    publish / skip := true,
    mimaFailOnNoPrevious := false,
    addCommandAlias("ci", "+test;+mimaReportBinaryIssues;+publishLocal;writeVersion"),
    writeVersion := {
      IO.write(file(".version"), version.value)
    },
  )
  .aggregate(
    ast,
    source,
    core,
    parser,
    formatter,
    languageSupport,
    lsp,
    pluginCore,
    pluginSample,
    e2e,
  )
