import scala.sys.process._

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val open = taskKey[Unit]("open vscode")

lazy val root = project
  .in(file("."))
  .settings(
    scalaVersion := "2.13.7",
    moduleName := "smithy-playground-vscode",
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
    // install with npmInstallDependencies
    Compile / npmDependencies ++= Seq(
      "@types/vscode" -> "1.63.1"
    ),
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "0.7.29" % Test
    ),
    scalacOptions -= "-Xfatal-warnings",
    Compile / fastOptJS / artifactPath := baseDirectory.value / "out" / "extension.js",
    Compile / fullOptJS / artifactPath := baseDirectory.value / "out" / "extension.js",
  )
  .enablePlugins(
    ScalaJSPlugin,
    ScalaJSBundlerPlugin,
    ScalablyTypedConverterPlugin,
  )
