package colang.tokens

import colang.Strategy.Result.Success
import colang.TestUtils._
import colang.{SourceCode, SourceCodeStream, UnitSpec}

class PunctuationLikeTokenStrategiesSpec extends UnitSpec {

  def describeStatelessStrategy[T <: Token](tokenText: String,
                                            tokenClass: Class[T],
                                            tokenStrategy: LexerImpl.StatelessTokenStrategy[T]): Unit = {

    val sourceFile = new InlineSourceFile("stateless-tokens.co", tokenText * 2 + " ")

    describe(s"'$tokenText' lexer strategy") {
      it(s"should match single '$tokenText' tokens") {
        val streamAtToken = new SourceCodeStream(sourceFile, 0, 0, 0)

        inside(tokenStrategy(streamAtToken)) { case Success(token, issues, newStream) =>
          token.getClass should be (tokenClass)

          val endChar = tokenText.length - 1
          token.source should matchPattern { case SourceCode(`sourceFile`, 0, 0, 0, `endChar`) => }

          issues shouldBe empty

          newStream.file should be (sourceFile)
          newStream.startChar should be (tokenText.length)
        }
      }
    }
  }

  describeStatelessStrategy("=", classOf[Assign], Assign.strategy)
  describeStatelessStrategy("==", classOf[Equals], Equals.strategy)
  describeStatelessStrategy("+", classOf[Plus], Plus.strategy)
  describeStatelessStrategy(",", classOf[Comma], Comma.strategy)
  describeStatelessStrategy("{", classOf[LeftBrace], LeftBrace.strategy)
  describeStatelessStrategy("(", classOf[LeftParen], LeftParen.strategy)
  describeStatelessStrategy("}", classOf[RightBrace], RightBrace.strategy)
  describeStatelessStrategy(")", classOf[RightParen], RightParen.strategy)
  describeStatelessStrategy(";", classOf[Semicolon], Semicolon.strategy)
}
