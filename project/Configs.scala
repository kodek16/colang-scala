import sbt._

object Configs {
  val EndToEndTest = config("e2e") extend Runtime
}
