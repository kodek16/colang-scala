package colang.tokens

import colang.{SourceCode, UnitSpec}
import colang.TestUtils._
import colang.issues.Error
import org.scalatest.LoneElement._

class LexerImplSpec extends UnitSpec {

  val sourceFile = new InlineSourceFile("example.co",
    """void main() {
      |    ошибка
      |}
    """.stripMargin)

  val lexer = new LexerImpl

  describe("A lexer implementation") {
    it("should split source code into tokens correctly") {
      val (tokens, issues) = lexer.splitIntoTokens(sourceFile)

      tokens should matchPattern { case Seq(
        Identifier("void", _),
        Whitespace(false, _),
        Identifier("main", _),
        LeftParen(_),
        RightParen(_),
        Whitespace(false, _),
        LeftBrace(_),
        Whitespace(true, _),
        Whitespace(true, _),
        RightBrace(_)) => }

      issues.loneElement should matchPattern { case Error(_, SourceCode(`sourceFile`, 1, 4, 1, 9), _, _) => }
    }
  }
}
