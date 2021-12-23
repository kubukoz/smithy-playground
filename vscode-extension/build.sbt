import scala.sys.process._

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val open = taskKey[Unit]("open vscode")

lazy val root = project
  .in(file("."))
  .settings(
    scalaVersion := "2.13.7",
    moduleName := "smithy-playground-vscode",
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
    externalNpm := {
      Process("yarn", baseDirectory.value).!
      baseDirectory.value
    },
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % "3.3.1",
      "org.scalameta" %%% "munit" % "0.7.29" % Test,
    ),
    scalacOptions -= "-Xfatal-warnings",
    Compile / fastOptJS / artifactPath := baseDirectory.value / "out" / "extension.js",
    Compile / fullOptJS / artifactPath := baseDirectory.value / "out" / "extension.js",
  )
  .enablePlugins(
    ScalaJSPlugin,
    ScalablyTypedConverterExternalNpmPlugin,
  )
