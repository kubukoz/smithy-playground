inThisBuild(
  List(
    organization := "com.kubukoz",
    homepage := Some(url("https://github.com/kubukoz/smithy-playground")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
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

import scala.sys.process._

def crossPlugin(x: sbt.librarymanagement.ModuleID) = compilerPlugin(x.cross(CrossVersion.full))

val compilerPlugins =
  libraryDependencies ++= List(
    crossPlugin("org.polyvariant" % "better-tostring" % "0.3.17")
  ) ++ (if (scalaVersion.value.startsWith("3"))
          Nil
        else
          List(
            crossPlugin("org.typelevel" % "kind-projector" % "0.13.2")
          ))

ThisBuild / versionScheme := Some("early-semver")

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := "2.13.10"
ThisBuild / crossScalaVersions := Seq("2.13.10")

// For coursier's "latest.integration"
ThisBuild / dynverSeparator := "-"

val commonSettings = Seq(
  organization := "com.kubukoz.playground",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.9.0",
    "org.typelevel" %% "cats-mtl" % "1.3.0",
    "com.disneystreaming" %% "weaver-cats" % "0.8.1" % Test,
    "com.disneystreaming" %% "weaver-discipline" % "0.8.1" % Test,
    "com.disneystreaming" %% "weaver-scalacheck" % "0.8.1" % Test,
    "com.softwaremill.diffx" %% "diffx-core" % "0.8.2" % Test,
    "com.softwaremill.diffx" %% "diffx-cats" % "0.8.2" % Test,
  ),
  testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
  compilerPlugins,
  scalacOptions -= "-Xfatal-warnings",
  scalacOptions -= "-Vtype-diffs",
  scalacOptions += "-Wnonunit-statement",
  scalacOptions ++= Seq("-Xsource:3.0"),
  javacOptions ++= Seq("-source", "8", "-target", "8"),
  mimaFailOnNoPrevious := false,
)

def module(name: String) = Project(name, file("modules") / name)
  .settings(
    commonSettings
  )

// Plugin interface. Keeps binary compatibility guarantees (mostly tied to smithy4s's bincompat).
lazy val pluginCore = module("plugin-core").settings(
  libraryDependencies ++= Seq(
    "com.disneystreaming.smithy4s" %% "smithy4s-http4s" % smithy4sVersion.value
  ),
  mimaPreviousArtifacts := Set(organization.value %% name.value % "0.3.0"),
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
      "org.typelevel" %% "cats-parse" % "0.3.9",
      "io.circe" %% "circe-generic" % "0.14.3" % Test,
      "io.circe" %% "circe-parser" % "0.14.3" % Test,
      "co.fs2" %% "fs2-io" % "3.6.0" % Test,
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
      "org.typelevel" %% "paiges-cats" % "0.4.2"
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
      "org.typelevel" %% "cats-effect" % "3.4.6",
      "com.disneystreaming.smithy4s" %% "smithy4s-dynamic" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" %% "smithy4s-http4s" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" %% "smithy4s-aws-http4s" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" %% "smithy4s-codegen-cli" % smithy4sVersion.value % Test,
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
      "com.disneystreaming.smithy4s" %% "smithy4s-codegen" % smithy4sVersion.value,
      "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.19.0",
      "io.circe" %% "circe-core" % "0.14.3",
      "org.http4s" %% "http4s-ember-client" % "0.23.18",
      "org.http4s" %% "http4s-ember-server" % "0.23.18" % Test,
      "io.get-coursier" %% "coursier" % "2.0.16",
      "org.typelevel" %% "cats-tagless-macros" % "0.14.0",
    ),
    buildInfoPackage := "playground.lsp.buildinfo",
    buildInfoKeys ++= Seq(version),
    Smithy4sCodegenPlugin.defaultSettings(Test),
    Test / smithy4sSmithyLibrary := false,
  )
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(languageSupport)

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
  .aggregate(ast, source, core, parser, formatter, languageSupport, lsp, pluginCore)
