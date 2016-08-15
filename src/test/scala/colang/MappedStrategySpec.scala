package colang

import colang.TestUtils._
import colang.Strategy.Result.{Malformed, NoMatch, Success}

class MappedStrategySpec extends UnitSpec {

  case class WrappedValue[T](value: T)
  def wrap[T](t: T) = WrappedValue(t)

  val issue = makeWarning("some warning")

  val successfulStrategy = makeSuccessfulStrategy(4, Seq(issue), "new stream")
  val malformedStrategy = makeMalformedStrategy[String, Int](Seq(issue), "new stream")
  val noMatchStrategy = makeNonMatchingStrategy[String, Int]

  describe("A mapped strategy") {
    it("should wrap objects produced by underlying strategy") {
      val mappedStrategy = MappedStrategy(successfulStrategy, wrap[Int])
      mappedStrategy("") should matchPattern { case Success(WrappedValue(4), Seq(`issue`), "new stream") => }
    }

    it("should return Malformed when underlying strategy returns Malformed") {
      val mappedStrategy = MappedStrategy(malformedStrategy, wrap[Int])
      mappedStrategy("") should matchPattern { case Malformed(Seq(`issue`), "new stream") => }
    }

    it("should not match when underlying strategy doesn't") {
      val mappedStrategy = MappedStrategy(noMatchStrategy, wrap[Int])
      mappedStrategy("") should matchPattern { case NoMatch() => }
    }
  }
}
