package colang.tokens

import colang.Strategy.Result.{Malformed, NoMatch}
import colang.{SourceCodeStream, UnitSpec}
import colang.TestUtils._

class LineCommentStrategySpec extends UnitSpec {

  val sourceFile = new InlineSourceFile("line-comment.co", "op; //comment\nnextOp / 2")

  describe("Line comment lexer strategy") {
    it("should match line comments starting with '//'") {
      val streamAtComment = new SourceCodeStream(sourceFile, 4, 0, 4)

      inside(LexerImpl.lineCommentStrategy(streamAtComment)) { case Malformed(issues, newStream) =>
        issues shouldBe empty

        newStream.file should be (sourceFile)
        newStream.startChar should be (13)
      }
    }

    it("should not match single forward slashes") {
      val streamAtDiv = new SourceCodeStream(sourceFile, 21, 2, 7)
      LexerImpl.lineCommentStrategy(streamAtDiv) should matchPattern { case NoMatch() => }
    }
  }
}
