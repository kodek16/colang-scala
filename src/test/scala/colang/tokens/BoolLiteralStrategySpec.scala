package colang.tokens

import colang.LexerUnitSpec

class BoolLiteralStrategySpec extends LexerUnitSpec {

  describe("Bool literal lexer strategy") {
    it("should match 'true'") {
      BoolLiteral.strategy shouldSucceedOn "true" withoutIssues() andInProduced { token =>
        token.value should be (true)
      }
    }

    it("should match 'false'") {
      BoolLiteral.strategy shouldSucceedOn "false" withoutIssues() andInProduced { token =>
        token.value should be (false)
      }
    }

    it("should not match identifiers that 'true' or 'false' is a prefix of") {
      BoolLiteral.strategy shouldNotMatch "trueabc"
      BoolLiteral.strategy shouldNotMatch "falsexyz"
    }
  }
}
