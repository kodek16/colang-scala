package colang.tokens

import colang.Strategy.Result.{NoMatch, Success}
import colang.TestUtils._
import colang.{SourceCode, SourceCodeStream, UnitSpec}

class KeywordStrategiesSpec extends UnitSpec {

  def describeKeywordStrategy[K <: Keyword](keywordText: String,
                                            keywordClass: Class[K],
                                            keywordStrategy: LexerImpl.StatelessTokenStrategy[K]): Unit = {

    val sourceFile = new InlineSourceFile("keywords.co", s"$keywordText ${keywordText}abc")

    describe(s"'$keywordText' keyword lexer strategy") {
      it(s"should match '$keywordText' words") {
        val streamAtKeyword = new SourceCodeStream(sourceFile, 0, 0, 0)
        inside(keywordStrategy(streamAtKeyword)) { case Success(token, issues, newStream) =>
          token.getClass should be (keywordClass)

          val endChar = keywordText.length - 1
          token.source should matchPattern { case SourceCode(`sourceFile`, 0, 0, 0, `endChar`) => }

          issues shouldBe empty

          newStream.file should be (sourceFile)
          newStream.startChar should be (keywordText.length)
        }
      }

      it(s"should not match other identifiers that '$keywordText' is a prefix of") {
        val streamAtIdentifier = new SourceCodeStream(sourceFile, keywordText.length + 1, 0, keywordText.length + 1)
        keywordStrategy(streamAtIdentifier) should matchPattern { case NoMatch() => }
      }
    }
  }

  describeKeywordStrategy("native", classOf[NativeKeyword], NativeKeyword.strategy)
  describeKeywordStrategy("struct", classOf[StructKeyword], StructKeyword.strategy)
}
