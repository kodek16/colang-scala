package colang.tokens

import colang.LexerUnitSpec

class IntLiteralStrategySpec extends LexerUnitSpec {

  describe("Int literal lexer strategy") {
    it("should match simple positive integers (like 2)") {
      IntLiteral.strategy shouldSucceedOn "2" withoutIssues() andInProduced { token =>
        token.value should be (2)
      }
    }

    it("should match simple negative integers (like -5)") {
      IntLiteral.strategy shouldSucceedOn "-5" withoutIssues() andInProduced { token =>
        token.value should be (-5)
      }
    }

    it("should match integers in scientific notation (like -2e6)") {
      IntLiteral.strategy shouldSucceedOn "-2e6" withoutIssues() andInProduced { token =>
        token.value should be (-2000000)
      }
    }

    it("should generate an error for \"integers\" with non-natural exponents (like 1e-5.6)") {
      IntLiteral.strategy shouldSucceedOn "1e-5.6" withOneError()
    }

    it("should generate an error for integers that are outside 'int' type value range") {
      IntLiteral.strategy shouldSucceedOn "12345678901" withOneError()
      IntLiteral.strategy shouldSucceedOn "-12345678901" withOneError()
    }

    it("should generate an error for integers in scientific notation that are outside of 'int' value range") {
      IntLiteral.strategy shouldSucceedOn "2e10" withOneError()
      IntLiteral.strategy shouldSucceedOn "-2e10" withOneError()
    }

    it("should process corner cases (INT_MIN - 1, INT_MIN, INT_MAX, INT_MAX + 1) correctly") {
      IntLiteral.strategy shouldSucceedOn "-2147483649" withOneError()
      IntLiteral.strategy shouldSucceedOn "-2147483648" withoutIssues() andInProduced { token =>
        token.value should be (-2147483648)
      }
      IntLiteral.strategy shouldSucceedOn "2147483647" withoutIssues() andInProduced { token =>
        token.value should be (2147483647)
      }
      IntLiteral.strategy shouldSucceedOn "2147483648" withOneError()
    }
  }
}
