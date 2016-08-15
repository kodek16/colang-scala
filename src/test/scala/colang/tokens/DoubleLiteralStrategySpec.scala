package colang.tokens

import colang.Strategy.Result.{NoMatch, Success}
import colang.TestUtils._
import colang.{SourceCode, SourceCodeStream, UnitSpec}

class DoubleLiteralStrategySpec extends UnitSpec {

  val sourceFile = new InlineSourceFile("doubles.co", """3.14 3,14""")

  describe("Double literal lexer strategy") {
    it("should match literals of form 3.14") {
      val streamAt3_14 = new SourceCodeStream(sourceFile, 0, 0, 0)

      inside(DoubleLiteral.strategy(streamAt3_14)) { case Success(token, issues, newStream) =>
        token should matchPattern { case DoubleLiteral(3.14, SourceCode(`sourceFile`, 0, 0, 0, 3)) => }
        issues shouldBe empty

        newStream.file should be (sourceFile)
        newStream.startChar should be (4)
      }
    }

    it("should not match literals with fractional part separated by comma") {
      val streamAt3_14 = new SourceCodeStream(sourceFile, 5, 0, 5)
      DoubleLiteral.strategy(streamAt3_14) should matchPattern { case NoMatch() => }
    }
  }
}
