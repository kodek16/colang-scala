package colang.tokens

import colang.Strategy.Result.{Malformed, NoMatch}
import colang.{SourceCodeStream, UnitSpec}
import colang.TestUtils._

class BlockCommentStrategySpec extends UnitSpec {

  val sourceFile = new InlineSourceFile("block-comment.co", "op /* block \n comment */ nextOp / * 2")

  describe("Block comment lexer strategy") {
    it("should match block comments starting with '/*' and ending with '*/'") {
      val streamAtComment = new SourceCodeStream(sourceFile, 3, 0, 3)

      inside(LexerImpl.blockCommentStrategy(streamAtComment)) { case Malformed(issues, newStream) =>
        issues shouldBe empty

        newStream.file should be (sourceFile)
        newStream.startChar should be (24)
      }
    }

    it("should not match separate '/ *' character sequence") {
      val streamAtSeparate = new SourceCodeStream(sourceFile, 32, 1, 19)
      streamAtSeparate.charAt(0) should be ('/')

      LexerImpl.blockCommentStrategy(streamAtSeparate) should matchPattern { case NoMatch() => }
    }
  }
}