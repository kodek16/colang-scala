lazy val compiler = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  configs(Configs.EndToEndTest).
  settings(Testing.settings: _*).
  settings(
    organization := "colang",
    name := "colang",
    scalaVersion := "2.12.3",
    version := "0.1-SNAPSHOT",

    buildInfoKeys := Seq[BuildInfoKey](version),
    buildInfoPackage := "colang",
  )

// Assembly configuration
mainClass in assembly := Some("colang.Compiler")
test in assembly := Testing.testAll.value

scalacOptions ++= Seq("-feature", "-language:postfixOps")

resolvers += Resolver.sonatypeRepo("public")

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "3.7.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalatest" %% "scalatest" % "3.0.1" % "e2e",
)
