package colang.tokens

import colang.LexerUnitSpec

class UnknownTokenStrategySpec extends LexerUnitSpec {

  describe("Unknown token lexer strategy") {
    it("should match any non-whitespace character sequence with an error") {
      LexerImpl.unknownTokenStrategy shouldMatchMalformedOn "$#@!ящ" withOneError "E0004"
    }

    it("should stop at whitespace") {
      LexerImpl.unknownTokenStrategy shouldOnlyMatchMalformedOn "$#@!ящ" from "$#@!ящ next" withOneError "E0004"
    }
  }
}
