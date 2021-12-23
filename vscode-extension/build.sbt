import scala.sys.process._

lazy val installDependencies = Def.task[Unit] {
  val base = (ThisProject / baseDirectory).value
  val log = (ThisProject / streams).value.log
  if (!(base / "node_module").exists) {
    val pb = new java.lang.ProcessBuilder("npm", "install")
      .directory(base)
      .redirectErrorStream(true)

    pb ! log
  }
}

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val open = taskKey[Unit]("open vscode")

def openVSCodeTask: Def.Initialize[Task[Unit]] = Def
  .task[Unit] {
    val base = (ThisProject / baseDirectory).value
    val log = (ThisProject / streams).value.log

    val path = base.getCanonicalPath
    s"code --extensionDevelopmentPath=$path" ! log
    ()
  }
  .dependsOn(installDependencies)

lazy val root = project
  .in(file("."))
  .settings(
    scalaVersion := "2.13.7",
    moduleName := "smithy-playground-vscode",
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
    open := openVSCodeTask.dependsOn(Compile / fastOptJS).value,
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
