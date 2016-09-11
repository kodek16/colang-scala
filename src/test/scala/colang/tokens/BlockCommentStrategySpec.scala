package colang.tokens

import colang.LexerUnitSpec

class BlockCommentStrategySpec extends LexerUnitSpec {

  describe("Block comment lexer strategy") {
    it("should match block comments starting with '/*' and ending with '*/'") {
      LexerImpl.blockCommentStrategy shouldMatchMalformedOn "/* comment */" withoutIssues()
    }

    it("should not match text after closing '*/'") {
      LexerImpl.blockCommentStrategy shouldOnlyMatchMalformedOn "/* comment */" from "/* comment */ abc" withoutIssues()
    }

    it("should not match separate '/ *' character sequence") {
      LexerImpl.blockCommentStrategy shouldNotMatch "/ * not really comment */"
    }
  }
}
