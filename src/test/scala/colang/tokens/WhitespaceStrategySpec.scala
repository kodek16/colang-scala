package colang.tokens

import colang.LexerUnitSpec

class WhitespaceStrategySpec extends LexerUnitSpec {

  describe("Whitespace lexer strategy") {
    it("should match longest sequence of spaces, tabs, and line breaks") {
      Whitespace.strategy shouldSucceedOn " \t\t \n\t \n " withoutIssues() andInProduced { token =>
        token.hasLineBreaks should be (true)
      }
    }

    it("should set 'hasLineBreaks' property to false if there weren't any") {
      Whitespace.strategy shouldSucceedOn " \t \t\t  " withoutIssues() andInProduced { token =>
        token.hasLineBreaks should be (false)
      }
    }

    it("should not match non-whitespace characters") {
      Whitespace.strategy shouldNotMatch "hello world"
    }
  }
}
