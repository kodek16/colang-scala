package colang.tokens

import colang.Strategy.Result.{NoMatch, Success}
import colang.{SourceCode, SourceCodeStream, UnitSpec}
import colang.TestUtils._

class WhitespaceStrategySpec extends UnitSpec {

  val sourceFile = new InlineSourceFile("whitespace.co", " \t\t \n\nx\t\t  y")

  describe("Whitespace lexer strategy") {
    it("should match longest sequence of spaces, tabs, and line breaks") {
      val streamAtWs = new SourceCodeStream(sourceFile, 0, 0, 0)

      inside(Whitespace.strategy(streamAtWs)) { case Success(token, issues, newStream) =>
        token should matchPattern { case Whitespace(true, SourceCode(`sourceFile`, 0, 0, 1, 0)) => }
        issues shouldBe empty

        newStream.file should be (sourceFile)
        newStream.startChar should be (6)
      }
    }

    it("should set 'hasLineBreaks' property to false if there weren't any") {
      val streamAtWs = new SourceCodeStream(sourceFile, 7, 2, 1)

      inside(Whitespace.strategy(streamAtWs)) { case Success(token, issues, newStream) =>
        token should matchPattern { case Whitespace(false, SourceCode(`sourceFile`, 2, 1, 2, 4)) => }
        issues shouldBe empty

        newStream.file should be (sourceFile)
        newStream.startChar should be (11)
      }
    }

    it("should not match non-whitespace characters") {
      val streamAtLetter = new SourceCodeStream(sourceFile, 6, 2, 0)
      Whitespace.strategy(streamAtLetter) should matchPattern { case NoMatch() => }
    }
  }
}
