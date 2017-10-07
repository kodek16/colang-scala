import sbt._
import sbt.Keys._

import Configs._

object Testing {
  lazy val testAll = TaskKey[Unit]("test-all")

  private lazy val e2eSettings =
    inConfig(EndToEndTest)(Defaults.testSettings) ++
    Seq(
      fork in EndToEndTest := false,
      parallelExecution in EndToEndTest := false,
      scalaSource in EndToEndTest := baseDirectory.value / "src/e2e/scala")

  lazy val settings = e2eSettings ++ Seq(
    testAll := {
      (test in Test).value
      (test in EndToEndTest).value
    }
  )
}
