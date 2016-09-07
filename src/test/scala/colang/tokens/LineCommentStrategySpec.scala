package colang.tokens

import colang.LexerUnitSpec

class LineCommentStrategySpec extends LexerUnitSpec {

  describe("Line comment lexer strategy") {
    it("should match line comments starting with '//'") {
      LexerImpl.lineCommentStrategy shouldMatchMalformedOn "// line comment" withoutIssues()
    }

    it("should not match next lines") {
      LexerImpl.lineCommentStrategy shouldOnlyMatchMalformedOn "//comment" from "//comment\n next line" withoutIssues()
    }

    it("should not match single forward slashes") {
      LexerImpl.lineCommentStrategy shouldNotMatch "/ 0"
    }
  }
}
