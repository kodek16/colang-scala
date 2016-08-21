package colang.tokens

import colang.LexerUnitSpec

class OperatorStrategiesSpec extends LexerUnitSpec {

  describe("'!' lexer strategy") {
    it("should match single '!' tokens") {
      LogicalNot.strategy shouldSucceedOn "!" withoutIssues()
    }

    it("should match sequential '!' tokens one-by-one") {
      LogicalNot.strategy shouldOnlySucceedOn "!" from "!!" withoutIssues()
    }

    it("should not match '!' from '!=' operator") {
      LogicalNot.strategy shouldNotMatch "!="
    }
  }

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

  describe("'<' lexer strategy") {
    it("should match single '<' tokens") {
      Less.strategy shouldSucceedOn "<" withoutIssues()
    }

    it("should not match multiple '<' tokens without whitespace") {
      Less.strategy shouldNotMatch "<<"
    }
  }

  describe("'>' lexer strategy") {
    it("should match single '>' tokens") {
      Greater.strategy shouldSucceedOn ">" withoutIssues()
    }

    it("should not match multiple '>' tokens without whitespace") {
      Greater.strategy shouldNotMatch ">>"
    }
  }

  describe("'<=' lexer strategy") {
    it("should match separated '<=' tokens") {
      LessOrEquals.strategy shouldSucceedOn "<=" withoutIssues()
    }

    it("should not match '<' followed by two or more '=' tokens without whitespace") {
      LessOrEquals.strategy shouldNotMatch "<=="
      LessOrEquals.strategy shouldNotMatch "<==="
    }
  }

  describe("'>=' lexer strategy") {
    it("should match separated '>=' tokens") {
      GreaterOrEquals.strategy shouldSucceedOn ">=" withoutIssues()
    }

    it("should not match '>' followed by two or more '=' tokens without whitespace") {
      GreaterOrEquals.strategy shouldNotMatch ">=="
      GreaterOrEquals.strategy shouldNotMatch ">==="
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

  describe("'!=' lexer strategy") {
    it("should match separated '!=' tokens") {
      NotEquals.strategy shouldSucceedOn "!=" withoutIssues()
    }

    it("should not match '!' followed by two or more '=' tokens without whitespace") {
      NotEquals.strategy shouldNotMatch "!=="
      NotEquals.strategy shouldNotMatch "!==="
    }
  }

  describe("'&&' lexer strategy") {
    it("should match separated '&&' tokens") {
      LogicalAnd.strategy shouldSucceedOn "&&" withoutIssues()
    }

    it("should not match three or more '&' tokens without whitespace") {
      LogicalAnd.strategy shouldNotMatch "&&&"
      LogicalAnd.strategy shouldNotMatch "&&&&"
    }
  }

  describe("'||' lexer strategy") {
    it("should match separated '||' tokens") {
      LogicalOr.strategy shouldSucceedOn "||" withoutIssues()
    }

    it("should not match three or more '|' tokens without whitespace") {
      LogicalOr.strategy shouldNotMatch "|||"
      LogicalOr.strategy shouldNotMatch "||||"
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
