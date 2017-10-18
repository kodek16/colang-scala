package colang

import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.issues.Warning

class MappedStrategySpec extends LexerUnitSpec {

  private case class WrappedValue[T](value: T)
  private def wrap[T](t: T) = WrappedValue(t)

  private val issue = Warning("W0001", SourceCode(new InlineSourceFile(""), 0, 0, 0, 0), "", Seq.empty)

  private val successfulStrategy = makeSuccessfulStrategy(4, Seq(issue), "new stream")
  private val malformedStrategy = makeMalformedStrategy[String, Int](Seq(issue), "new stream")
  private val noMatchStrategy = makeNonMatchingStrategy[String, Int]

  describe("A mapped strategy") {
    it("should wrap objects produced by underlying strategy") {
      val mappedStrategy = MappedStrategy(successfulStrategy, wrap[Int])
      mappedStrategy("") should matchPattern { case Success(WrappedValue(4), Seq(`issue`), "new stream") => }
    }

    it("should return Skipped when underlying strategy returns Skipped") {
      val mappedStrategy = MappedStrategy(malformedStrategy, wrap[Int])
      mappedStrategy("") should matchPattern { case Malformed(Seq(`issue`), "new stream") => }
    }

    it("should not match when underlying strategy doesn't") {
      val mappedStrategy = MappedStrategy(noMatchStrategy, wrap[Int])
      mappedStrategy("") should matchPattern { case NoMatch() => }
    }
  }
}
