resolvers += "OSS Maven" at "https://artifactory.us-east-1.bamgrid.net/artifactory/oss-maven/"
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.20")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.7")

addDependencyTreePlugin
