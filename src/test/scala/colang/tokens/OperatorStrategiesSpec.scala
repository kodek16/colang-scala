package colang.tokens

import colang.LexerUnitSpec

class OperatorStrategiesSpec extends LexerUnitSpec {

  describe("'*' lexer strategy") {
    it("should match single '*' tokens") {
      Multiply.strategy shouldSucceedOn "*" withoutIssues()
    }

    it("should not match multiple '*' tokens without whitespace") {
      Multiply.strategy shouldNotMatch "**"
    }
  }

  describe("'/' lexer strategy") {
    it("should match single '/' tokens") {
      Divide.strategy shouldSucceedOn "/" withoutIssues()
    }

    it("should not match multiple '/' tokens without whitespace") {
      Divide.strategy shouldNotMatch "//"
    }
  }

  describe("'+' lexer strategy") {
    it("should match single '+' tokens") {
      Plus.strategy shouldSucceedOn "+" withoutIssues()
    }

    it("should not match multiple '+' tokens without whitespace") {
      Plus.strategy shouldNotMatch "++"
    }
  }

  describe("'-' lexer strategy") {
    it("should match single '-' tokens") {
      Minus.strategy shouldSucceedOn "-" withoutIssues()
    }

    it("should not match multiple '-' tokens without whitespace") {
      Minus.strategy shouldNotMatch "--"
    }
  }

  describe("'==' lexer strategy") {
    it("should match separated '==' tokens") {
      Equals.strategy shouldSucceedOn "==" withoutIssues()
    }

    it("should not match three or more '=' tokens without whitespace") {
      Equals.strategy shouldNotMatch "==="
      Equals.strategy shouldNotMatch "===="
    }
  }

  describe("'=' lexer strategy") {
    it("should match single '=' tokens") {
      Assign.strategy shouldSucceedOn "=" withoutIssues()
    }

    it("should not match multiple '=' tokens without whitespace") {
      Assign.strategy shouldNotMatch "=="
    }
  }
}
