package colang

import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.tokens.{LexerImpl, Token}

abstract class LexerUnitSpec extends UnitSpec {

  /**
    * A source file mock for inline construction.
    * @param text raw source code
    */
  class InlineSourceFile(val text: String) extends SourceFile {
    val name = "<inlined source>"
  }

  /**
    * Wraps a successful result allowing further checks.
    * @param token parsed token
    * @param issues encountered issues
    */
  class SuccessWrapper[T](token: T, issues: Seq[Issue]) {

    /**
      * Terminates the check chain with a function that checks the parsed token itself.
      * @param f function that checks the token
      */
    def andInProduced(f: T => Unit) = f(token)

    /**
      * Asserts that there were no issues. Scala syntax requires that empty parentheses are present when calling this
      * method.
      * @return this
      */
    def withoutIssues(): SuccessWrapper[T] = {
      if (issues.nonEmpty) {
        fail("Strategy matched successfully, but produced issues though it was not supposed to.")
      }
      this
    }

    /**
      * Asserts that there was exactly one error. Scala syntax requires that empty parentheses are present when calling
      * this method.
      * @return
      */
    def withOneError(): SuccessWrapper[T] = {
      val errorCount = issues count { _.isInstanceOf[Error] }
      if (errorCount == 0) {
        fail("Strategy matched successfully without any errors though it was supposed to generate one error.")
      }
      if (errorCount >= 2) {
        fail("Strategy matched successfully with multiple errors though it was supposed to generate only one.")
      }
      this
    }
  }

  implicit class StrategyWrapper[T <: Token](strategy: LexerImpl.Strategy[T]) {

    /**
      * Asserts that the strategy matches a source code fragment completely, produces a correct new stream and
      * sets token source correctly. Further checks are usually chained on the return value, see SuccessWrapper.
      * @param source source code that the strategy should match
      * @return SuccessWrapper
      */
    def shouldSucceedOn(source: String): SuccessWrapper[T] = {
      val sourceFile = new InlineSourceFile(source)
      val stream = new SourceCodeStream(sourceFile, 0, 0, 0)

      strategy(stream) match {
        case Success(token, issues, streamAfterToken) =>
          val lastChar = source.length - 1

          token.source match {
            case SourceCode(`sourceFile`, 0, 0, 0, `lastChar`) => ()
            case _ =>
              fail("Strategy didn't match the whole character sequence.")
          }

          streamAfterToken.file should be (sourceFile)
          streamAfterToken.startChar should be (lastChar + 1)

          new SuccessWrapper(token, issues)
        case Malformed(_, _) =>
          fail("Strategy matched with Malformed when it was expected to succeed.")
        case NoMatch() =>
          fail("Strategy didn't match though it was supposed to.")
      }
    }
  }
}