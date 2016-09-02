package colang.tokens

import colang.Strategy.Result.{Malformed, NoMatch}
import colang.TestUtils._
import colang.issues.Error
import colang.{SourceCodeStream, UnitSpec}

class UnknownTokenStrategySpec extends UnitSpec {

  val sourceFile = new InlineSourceFile("unknown.co", "$#@!ящ normal text")

  describe("Unknown token lexer strategy") {
    it("should match any non-whitespace character sequence with an error") {
      val streamAtUnknown = new SourceCodeStream(sourceFile, 0, 0, 0)

      inside(LexerImpl.unknownTokenStrategy(streamAtUnknown)) { case Malformed(issues, newStream) =>
        forAtLeast(1, issues) { _ shouldBe an [Error] }

        newStream.file should be (sourceFile)
        newStream.charAt(0) should be (' ')
      }
    }

    it("should not match whitespace characters") {
      val streamAtWs = new SourceCodeStream(sourceFile, 6, 0, 6)
      LexerImpl.unknownTokenStrategy(streamAtWs) should matchPattern { case NoMatch() => }
    }
  }
}
