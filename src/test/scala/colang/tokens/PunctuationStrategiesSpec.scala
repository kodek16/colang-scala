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

  def describeStrictPunctuationStrategy[T <: Token](tokenText: String,
                                                    tokenStrategy: LexerImpl.StatelessTokenStrategy[T]): Unit = {

    describe(s"'$tokenText' lexer strategy") {
      it(s"should match single '$tokenText' tokens") {
        tokenStrategy shouldSucceedOn tokenText withoutIssues()
      }

      it(s"should not match '$tokenText$tokenText' tokens") {
        tokenStrategy shouldNotMatch (tokenText * 2)
      }
    }
  }

  describePunctuationStrategy("(", LeftParen.strategy)
  describePunctuationStrategy(")", RightParen.strategy)
  describePunctuationStrategy("{", LeftBrace.strategy)
  describePunctuationStrategy("}", RightBrace.strategy)
  describePunctuationStrategy(",", Comma.strategy)
  describePunctuationStrategy(";", Semicolon.strategy)

  describeStrictPunctuationStrategy(".", Dot.strategy)
  describeStrictPunctuationStrategy("&", Ampersand.strategy)
}
