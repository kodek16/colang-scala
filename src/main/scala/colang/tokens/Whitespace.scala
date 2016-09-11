package colang.tokens

import colang.Strategy.Result
import colang.Strategy.Result.{NoMatch, Success}
import colang.{SourceCode, SourceCodeStream}

/**
  * Represents a sequence of whitespace characters (' ', '\t', and '\n').
  * @param hasLineBreaks true if this sequence contains at least one line break
  */
case class Whitespace(hasLineBreaks: Boolean, source: SourceCode) extends Token

object Whitespace {
  val strategy = new LexerImpl.Strategy[Whitespace] {

    def apply(stream: SourceCodeStream): Result[SourceCodeStream, Whitespace] = {
      val re = """\s+""".r

      re findPrefixOf stream match {
        case Some(text) =>
          val (source, streamAfterToken) = stream.take(text)
          val token = Whitespace(text.contains("\n"), source)
          Success(token, Seq.empty, streamAfterToken)
        case None => NoMatch()
      }
    }
  }
}
