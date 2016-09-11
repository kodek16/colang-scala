package colang

import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.issues.{Error, Issue}
import colang.tokens.{LexerImpl, Token}

abstract class LexerUnitSpec extends UnitSpec {

  /**
    * A source file mock for inline construction.
    * @param rawText raw source code
    */
  class InlineSourceFile(rawText: String) extends SourceFile {
    val name = "<inlined source>"
    val text = rawText.replaceAll("\r\n", "\n")
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
          streamAfterToken.file should be (sourceFile)
          if (streamAfterToken.startChar != source.length) {
            fail("Strategy didn't match the whole character sequence.")
          }

          new SuccessWrapper(token, issues)
        case Malformed(_, _) =>
          fail("Strategy matched with Malformed when it was expected to succeed.")
        case NoMatch() =>
          fail("Strategy didn't match though it was supposed to.")
      }
    }

    /**
      * Asserts that the strategy matches a source code fragment completely and returns Malformed with a correct
      * new stream. Further checks are usually chained on the return value, see MalformedWrapper.
      * @param source source code that the strategy should match with Malformed
      * @return MalformedWrapper
      */
    def shouldMatchMalformedOn(source: String): MalformedWrapper = {
      val sourceFile = new InlineSourceFile(source)
      val stream = new SourceCodeStream(sourceFile, 0, 0, 0)

      strategy(stream) match {
        case Success(_, _, _) =>
          fail("Strategy succeeded when it was expected to match with Malformed.")
        case Malformed(issues, streamAfterToken) =>
          streamAfterToken.file should be (sourceFile)
          streamAfterToken.startChar should be (source.length)
          new MalformedWrapper(issues)
        case NoMatch() =>
          fail("Strategy didn't match though it was supposed to match with Malformed.")
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

    /**
      * Asserts that the strategy only matches a prefix of given source text and returns Malformed with a correct
      * new stream. Use it like 'strategy shouldOnlyMatchMalformedOn "prefix" from "prefix some other code".
      * Other MalformedWrapper assertions may follow.
      * @param tokenText token text that the strategy should match
      * @return PartialMalformedClause
      */
    def shouldOnlyMatchMalformedOn(tokenText: String) = new PartialMalformedClause(strategy, tokenText)
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
      * Asserts that there was exactly one error with a given code.
      * @param errorCode expected error code
      * @return
      */
    def withOneError(errorCode: String): SuccessWrapper[T] = {
      val errorCount = issues count { _.isInstanceOf[Error] }
      if (errorCount == 0) {
        fail("Strategy matched successfully without any errors though it was supposed to generate one error.")
      }
      if (errorCount >= 2) {
        fail("Strategy matched successfully with multiple errors though it was supposed to generate only one.")
      }
      val actualCode = issues.find { _.isInstanceOf[Error] }.get.code
      if (actualCode != errorCode) {
        fail(s"Expected error $errorCode, but got $actualCode.")
      }
      this
    }
  }

  /**
    * Wraps a Malformed result allowing additional issue checks.
    * @param issues encountered issues
    */
  class MalformedWrapper(issues: Seq[Issue]) {

    /**
      * Asserts that there were no issues. Scala syntax requires that empty parentheses are present when calling this
      * method.
      * @return this
      */
    def withoutIssues(): MalformedWrapper = {
      if (issues.nonEmpty) {
        fail("Strategy matched with Malformed, but produced issues though it was not supposed to.")
      }
      this
    }

    /**
      * Asserts that there was exactly one error with given code.
      * @param errorCode expected error code
      * @return
      */
    def withOneError(errorCode: String): MalformedWrapper = {
      val errorCount = issues count { _.isInstanceOf[Error] }
      if (errorCount == 0) {
        fail("Strategy matched without any errors though it was supposed to generate one error.")
      }
      if (errorCount >= 2) {
        fail("Strategy matched with multiple errors though it was supposed to generate only one.")
      }
      val actualCode = issues.find { _.isInstanceOf[Error] }.get.code
      if (actualCode != errorCode) {
        fail(s"Expected error $errorCode, but got $actualCode.")
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
          streamAfterToken.file should be (sourceFile)
          if (streamAfterToken.startChar == source.length) {
            fail("Strategy matched the whole sequence instead of only a part of it.")
          } else if (streamAfterToken.startChar != tokenText.length) {
            fail("Strategy didn't match the whole character sequence.")
          }

          new SuccessWrapper(token, issues)
        case Malformed(_, _) =>
          fail("Strategy matched with Malformed when it was expected to succeed.")
        case NoMatch() =>
          fail("Strategy didn't match though it was supposed to.")
      }
    }
  }

  class PartialMalformedClause[T <: Token](strategy: LexerImpl.Strategy[T], tokenText: String) {
    def from(source: String): MalformedWrapper = {
      val sourceFile = new InlineSourceFile(source)
      val stream = new SourceCodeStream(sourceFile, 0, 0, 0)

      strategy(stream) match {
        case Success(_, _, _) =>
          fail("Strategy succeeded when it was expected to match with Malformed.")
        case Malformed(issues, streamAfterToken) =>
          streamAfterToken.file should be (sourceFile)
          streamAfterToken.startChar should be (tokenText.length)
          new MalformedWrapper(issues)
        case NoMatch() =>
          fail("Strategy didn't match though it was supposed to match with Malformed.")
      }
    }
  }
}
