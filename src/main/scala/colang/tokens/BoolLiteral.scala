package colang.tokens

import colang.Strategy.Result
import colang.Strategy.Result.{NoMatch, Matched}
import colang.{SourceCode, SourceCodeStream}

/**
  * Represent a literal boolean value ('true' or 'false')
  * @param value literal value
  */
case class BoolLiteral(value: Boolean, source: SourceCode) extends Token

object BoolLiteral {
  val strategy = new LexerImpl.Strategy[BoolLiteral] {

    def apply(stream: SourceCodeStream): Result[SourceCodeStream, BoolLiteral] = {
      val re = """(true\b|false\b)""".r

      re findPrefixOf stream match {
        case Some(text) =>
          val (source, streamAfterToken) = stream.take(text)
          val token = BoolLiteral(text.toBoolean, source)
          Matched(token, Seq.empty, streamAfterToken)
        case None => NoMatch()
      }
    }
  }
}
