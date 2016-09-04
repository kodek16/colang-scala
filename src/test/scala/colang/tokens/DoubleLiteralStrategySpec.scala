package colang.tokens

import colang.LexerUnitSpec

class DoubleLiteralStrategySpec extends LexerUnitSpec {

  private val EPS = 1.0e-8

  describe("Double literal lexer strategy") {
    it("should match simple positive literals (like 3.14)") {
      DoubleLiteral.strategy shouldSucceedOn "3.14" withoutIssues() andInProduced { token =>
        token.value should be (3.14 +- EPS)
      }
    }

    it("should match simple negative literals (like -2.72)") {
      DoubleLiteral.strategy shouldSucceedOn "-2.72" withoutIssues() andInProduced { token =>
        token.value should be (-2.72 +- EPS)
      }
    }

    it("should match literals in scientific form with integer exponent") {
      DoubleLiteral.strategy shouldSucceedOn "-5.4e6" withoutIssues() andInProduced { token =>
        token.value should be (-5.4e6 +- EPS)
      }
    }

    it("should match literals in scientific form with negative exponents") {
      DoubleLiteral.strategy shouldSucceedOn "8.1E-9" withoutIssues() andInProduced { token =>
        token.value should be (8.1E-9 +- EPS)
      }
    }

    it("should not match literals with fractional exponents") {
      DoubleLiteral.strategy shouldNotMatch "1.0e2.5"
    }

    it("should not match integer literals") {
      DoubleLiteral.strategy shouldNotMatch "42"
    }

    it("should not match literals with fractional part separated by comma") {
      DoubleLiteral.strategy shouldNotMatch "3,14"
    }
  }
}
