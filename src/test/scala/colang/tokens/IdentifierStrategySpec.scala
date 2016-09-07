package colang.tokens

import colang.LexerUnitSpec

class IdentifierStrategySpec extends LexerUnitSpec {

  describe("Identifier lexer strategy") {
    it("should match simple alphabetic identifiers") {
      Identifier.strategy shouldSucceedOn "simple" withoutIssues() andInProduced { token =>
        token.value should be ("simple")
      }
    }

    it("should stop on whitespace") {
      Identifier.strategy shouldOnlySucceedOn "hello" from "hello world" withoutIssues() andInProduced { token =>
        token.value should be ("hello")
      }
    }

    it("should match complex identifiers starting with underscore") {
      Identifier.strategy shouldSucceedOn "_underscored2_4" withoutIssues() andInProduced { token =>
        token.value should be ("_underscored2_4")
      }
    }

    it("should not match tokens starting with a digit") {
      Identifier.strategy shouldNotMatch "4chan"
    }
  }
}
