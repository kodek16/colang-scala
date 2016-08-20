package colang.tokens

import colang.LexerUnitSpec

class UnknownNumberStrategySpec extends LexerUnitSpec {

  describe("Unknown number lexer strategy") {
    it("should match (with error) digits followed by a sequence of word characters and dots") {
      LexerImpl.unknownNumberStrategy shouldSucceedOn "3adz0.9._12_df." withOneError()
    }
  }
}
