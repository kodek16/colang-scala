package colang

import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.issues.{Error, Issue}
import colang.tokens.{LexerImpl, Token}

abstract class LexerUnitSpec extends UnitSpec {

  /**
    * A source file mock for inline construction.
    * @param text raw source code
    */
  class InlineSourceFile(val text: String) extends SourceFile {
    val name = "<inlined source>"
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

    /**
      * Asserts that the strategy doesn't match a source code fragment.
      * @param source source code that the strategy should not match
      */
    def shouldNotMatch(source: String): Unit = {
      val sourceFile = new InlineSourceFile(source)
      val stream = new SourceCodeStream(sourceFile, 0, 0, 0)

      strategy(stream) match {
        case Success(_, _, _) | Malformed(_, _) =>
          fail("Strategy matched though it was not supposed to.")
        case NoMatch() => ()
      }
    }

    /**
      * Asserts that the strategy only matches a prefix of given source text, and sets new positions correctly.
      * Use it like 'strategy shouldOnlySucceedOn "prefix" from "prefix some other code". Other SuccessWrapper
      * assertions may follow.
      * @param tokenText token text that the strategy should match
      * @return PartialSuccessClause
      */
    def shouldOnlySucceedOn(tokenText: String) = new PartialSuccessClause(strategy, tokenText)
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

  class PartialSuccessClause[T <: Token](strategy: LexerImpl.Strategy[T], tokenText: String) {
    def from(source: String): SuccessWrapper[T] = {
      val sourceFile = new InlineSourceFile(source)
      val stream = new SourceCodeStream(sourceFile, 0, 0, 0)

      strategy(stream) match {
        case Success(token, issues, streamAfterToken) =>
          val expectedlastChar = tokenText.length - 1

          token.source match {
            case SourceCode(`sourceFile`, 0, 0, 0, `expectedlastChar`) => ()
            case SourceCode(`sourceFile`, 0, 0, 0, actualLastChar) if actualLastChar == source.length - 1 =>
              fail("Strategy matched the whole sequence instead of only a part of it.")
            case _ =>
              fail("Strategy didn't match the whole character sequence.")
          }

          streamAfterToken.file should be (sourceFile)
          streamAfterToken.startChar should be (expectedlastChar + 1)

          new SuccessWrapper(token, issues)
        case Malformed(_, _) =>
          fail("Strategy matched with Malformed when it was expected to succeed.")
        case NoMatch() =>
          fail("Strategy didn't match though it was supposed to.")
      }
    }
  }
}