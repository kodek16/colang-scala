package colang

import colang.TestUtils._
import colang.Strategy.Result.{Malformed, NoMatch, Success}

class StrategyUnionSpec extends UnitSpec {

  val issueOne = makeWarning("some warning")
  val issueTwo = makeWarning("other warning")

  describe("A strategy union") {
    it("should choose first successful strategy") {
      val successfulOne = makeSuccessfulStrategy(1, Seq(issueOne), "new stream 1")
      val successfulTwo = makeSuccessfulStrategy(2, Seq(issueTwo), "new stream 2")

      val union = StrategyUnion(successfulOne, successfulTwo)
      union("") should matchPattern { case Success(1, Seq(`issueOne`), "new stream 1") => }
    }

    it("should skip strategies that didn't match") {
      val nonMatching = makeNonMatchingStrategy[String, Int]
      val successful = makeSuccessfulStrategy(1, Seq(issueOne), "new stream 1")

      val union = StrategyUnion(nonMatching, successful)
      union("") should matchPattern { case Success(1, Seq(`issueOne`), "new stream 1") => }
    }

    it("should choose strategy that returned Malformed over successful strategy that came later") {
      val malformed = makeMalformedStrategy(Seq(issueOne), "new stream 1")
      val successful = makeSuccessfulStrategy(2, Seq(issueTwo), "new stream 2")

      val union = StrategyUnion(malformed, successful)
      union("") should matchPattern { case Malformed(Seq(`issueOne`), "new stream 1") => }
    }

    it("should return NoMatch if none of the strategies matched") {
      val nonMatchingOne = makeNonMatchingStrategy[String, Int]
      val nonMatchingTwo = makeNonMatchingStrategy[String, Int]

      val union = StrategyUnion(nonMatchingOne, nonMatchingTwo)
      union("") should matchPattern { case NoMatch() => }
    }
  }
}
