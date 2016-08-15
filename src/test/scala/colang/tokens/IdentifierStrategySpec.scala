package colang.tokens

import colang.Strategy.Result.{NoMatch, Success}
import colang.TestUtils._
import colang.{SourceCode, SourceCodeStream, UnitSpec}

class IdentifierStrategySpec extends UnitSpec {

  val sourceFile = new InlineSourceFile("identifiers.co", """simple _underscored4 1337x""")

  describe("Identifier lexer strategy") {
    it("should match simple alphabetic identifiers") {
      val streamAtSimple = new SourceCodeStream(sourceFile, 0, 0, 0)

      inside(Identifier.strategy(streamAtSimple)) { case Success(token, issues, newStream) =>
        token should matchPattern { case Identifier("simple", SourceCode(`sourceFile`, 0, 0, 0, 5)) => }
        issues shouldBe empty

        newStream.file should be (sourceFile)
        newStream.startChar should be (6)
      }
    }

    it("should match complex identifiers starting with underscore") {
      val streamAtUnderscored = new SourceCodeStream(sourceFile, 7, 0, 7)

      inside(Identifier.strategy(streamAtUnderscored)) { case Success(token, issues, newStream) =>
        token should matchPattern { case Identifier("_underscored4", SourceCode(`sourceFile`, 0, 7, 0, 19)) => }
        issues shouldBe empty

        newStream.file should be (sourceFile)
        newStream.startChar should be (20)
      }
    }

    it("should not match tokens starting with a digit") {
      val streamAtNumber = new SourceCodeStream(sourceFile, 21, 0, 21)
      Identifier.strategy(streamAtNumber) should matchPattern { case NoMatch() => }
    }
  }
}
