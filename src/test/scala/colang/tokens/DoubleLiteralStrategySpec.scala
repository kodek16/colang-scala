package colang.tokens

import colang.Strategy.Result.{NoMatch, Success}
import colang.TestUtils._
import colang.{SourceCode, SourceCodeStream, UnitSpec}

class DoubleLiteralStrategySpec extends UnitSpec {

  val sourceFile = new InlineSourceFile("doubles.co", """3.14 3,14 -2.72 -5.4e6 4.1e-2.5""")

  describe("Double literal lexer strategy") {
    it("should match simple positive literals (like 3.14)") {
      val streamAt3_14 = new SourceCodeStream(sourceFile, 0, 0, 0)

      inside(DoubleLiteral.strategy(streamAt3_14)) { case Success(token, issues, newStream) =>
        token should matchPattern { case DoubleLiteral(3.14, SourceCode(`sourceFile`, 0, 0, 0, 3)) => }
        issues shouldBe empty

        newStream.file should be (sourceFile)
        newStream.startChar should be (4)
      }
    }

    it("should match simple negative literals (like -2.72)") {
      val streamAtNeg = new SourceCodeStream(sourceFile, 10, 0, 10)

      inside(DoubleLiteral.strategy(streamAtNeg)) { case Success(token, issues, newStream) =>
        token should matchPattern { case DoubleLiteral(-2.72, SourceCode(`sourceFile`, 0, 10, 0, 14)) => }
        issues shouldBe empty

        newStream.file should be (sourceFile)
        newStream.startChar should be (15)
      }
    }

    it("should match literals in scientific form with integer exponent") {
      val streamAtScientific = new SourceCodeStream(sourceFile, 16, 0, 16)

      inside(DoubleLiteral.strategy(streamAtScientific)) { case Success(token, issues, newStream) =>
        token should matchPattern { case DoubleLiteral(-5400000.0, SourceCode(`sourceFile`, 0, 16, 0, 21)) => }
        issues shouldBe empty

        newStream.file should be (sourceFile)
        newStream.startChar should be (22)
      }
    }

    it("should match literals in scientific form with fractional exponent") {
      val streamAtScientific = new SourceCodeStream(sourceFile, 23, 0, 23)

      inside(DoubleLiteral.strategy(streamAtScientific)) { case Success(token, issues, newStream) =>
        token.value should be ((4.1 * Math.pow(10, -2.5)) +- 1e-6)
        issues shouldBe empty

        newStream.file should be (sourceFile)
        newStream.startChar should be (31)
      }
    }

    it("should not match literals with fractional part separated by comma") {
      val streamAt3_14 = new SourceCodeStream(sourceFile, 5, 0, 5)
      DoubleLiteral.strategy(streamAt3_14) should matchPattern { case NoMatch() => }
    }
  }
}
