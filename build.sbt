lazy val compiler = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  configs(Configs.EndToEndTest).
  settings(Testing.settings :_*).
  settings(
    organization := "colang",
    name := "colang",
    scalaVersion := "2.11.8",
    version := "0.1-SNAPSHOT",
    mainClass in assembly := Some("colang.Compiler"),

    buildInfoKeys := Seq[BuildInfoKey](version),
    buildInfoPackage := "colang"
  )

scalacOptions ++= Seq("-feature", "-language:postfixOps")

resolvers += Resolver.sonatypeRepo("public")

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "3.5.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)