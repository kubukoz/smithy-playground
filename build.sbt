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

ThisBuild / scalaVersion := "2.13.8"
ThisBuild / crossScalaVersions := Seq("2.13.8")

val commonSettings = Seq(
  organization := "com.kubukoz.playground",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect" % "3.3.14",
    "com.disneystreaming" %% "weaver-cats" % "0.7.15" % Test,
    "com.disneystreaming" %% "weaver-discipline" % "0.7.15" % Test,
    "com.disneystreaming" %% "weaver-scalacheck" % "0.7.15" % Test,
  ),
  testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
  compilerPlugins,
  scalacOptions -= "-Xfatal-warnings",
  scalacOptions -= "-Vtype-diffs",
  scalacOptions ++= Seq("-Xsource:3.0"),
  javacOptions ++= Seq("-source", "8", "-target", "8"),
  mimaFailOnNoPrevious := false,
)

lazy val pluginCore = project.settings(
  name := "plugin-core",
  libraryDependencies ++= Seq(
    "com.disneystreaming.smithy4s" %% "smithy4s-http4s" % smithy4sVersion.value
  ),
  commonSettings,
  mimaPreviousArtifacts := Set(organization.value %% name.value % "0.3.0"),
)

lazy val core = project
  .settings(
    libraryDependencies ++= Seq(
      "com.disneystreaming.smithy4s" %% "smithy4s-dynamic" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" %% "smithy4s-http4s" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" %% "smithy4s-aws-http4s" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" %% "smithy4s-codegen-cli" % smithy4sVersion.value % Test,
      "org.typelevel" %% "cats-parse" % "0.3.8",
      "org.typelevel" %% "paiges-cats" % "0.4.2",
      "com.softwaremill.diffx" %% "diffx-core" % "0.7.1" % Test,
    ),
    commonSettings,
    Smithy4sCodegenPlugin.defaultSettings(Test),
  )
  .enablePlugins(Smithy4sCodegenPlugin)
  .dependsOn(pluginCore)

lazy val lsp = project
  .settings(
    libraryDependencies ++= Seq(
      "com.disneystreaming.smithy4s" %% "smithy4s-codegen" % smithy4sVersion.value,
      "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.15.0",
      "io.circe" %% "circe-core" % "0.14.2",
      "org.http4s" %% "http4s-ember-client" % "0.23.15",
      "io.get-coursier" %% "coursier" % "2.0.16",
      "org.typelevel" %% "cats-tagless-macros" % "0.14.0",
    ),
    commonSettings,
    buildInfoPackage := "playground.lsp.buildinfo",
    buildInfoKeys ++= Seq(version),
  )
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(core)

lazy val root = project
  .in(file("."))
  .settings(
    publish / skip := true,
    mimaFailOnNoPrevious := false,
    addCommandAlias("ci", "+test;+mimaReportBinaryIssues"),
  )
  .aggregate(core, lsp, pluginCore)
