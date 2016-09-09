package colang.tokens

import colang.LexerUnitSpec

class PunctuationStrategiesSpec extends LexerUnitSpec {

  def describePunctuationStrategy[T <: Token](tokenText: String,
                                              tokenStrategy: LexerImpl.StatelessTokenStrategy[T]): Unit = {

    describe(s"'$tokenText' lexer strategy") {
      it(s"should match single '$tokenText' tokens") {
        tokenStrategy shouldSucceedOn tokenText withoutIssues()
      }

      it(s"should match sequential '$tokenText' tokens one-by-one") {
        tokenStrategy shouldOnlySucceedOn tokenText from (tokenText * 2) withoutIssues()
      }
    }
  }

  describePunctuationStrategy("(", LeftParen.strategy)
  describePunctuationStrategy(")", RightParen.strategy)
  describePunctuationStrategy("{", LeftBrace.strategy)
  describePunctuationStrategy("}", RightBrace.strategy)
  describePunctuationStrategy(",", Comma.strategy)
  describePunctuationStrategy(";", Semicolon.strategy)

  // '&' strategy is different because it doesn't match sequential tokens.
  describe("'&' lexer strategy") {
    it("should match single '&' tokens") {
      Ampersand.strategy shouldSucceedOn "&" withoutIssues()
    }

    it("should not match '&&' tokens") {
      Ampersand.strategy shouldNotMatch "&&"
    }
  }
}
