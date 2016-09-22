package colang.tokens

import colang.LexerUnitSpec

class KeywordStrategiesSpec extends LexerUnitSpec {

  def describeKeywordStrategy[K <: Keyword](keywordText: String,
                                            keywordStrategy: LexerImpl.StatelessTokenStrategy[K]): Unit = {

    describe(s"'$keywordText' keyword lexer strategy") {
      it(s"should match single '$keywordText' words") {
        keywordStrategy shouldSucceedOn keywordText withoutIssues()
      }

      it(s"should not match other identifiers that '$keywordText' is a prefix of") {
        keywordStrategy shouldNotMatch  (keywordText + "abc")
      }
    }
  }

  describeKeywordStrategy("native", NativeKeyword.strategy)
  describeKeywordStrategy("struct", StructKeyword.strategy)
  describeKeywordStrategy("if", IfKeyword.strategy)
  describeKeywordStrategy("else", ElseKeyword.strategy)
  describeKeywordStrategy("while", WhileKeyword.strategy)
  describeKeywordStrategy("return", ReturnKeyword.strategy)
  describeKeywordStrategy("this", ThisKeyword.strategy)
  describeKeywordStrategy("constructor", ConstructorKeyword.strategy)
}
