package colang.tokens

import colang.Strategy.Result
import colang.Strategy.Result.{NoMatch, Success}
import colang.{SourceCode, SourceCodeStream}

/**
  * Represents a name that is meant to identify some symbol.
  * @param value literal name
  */
case class Identifier(value: String, source: SourceCode) extends Token

object Identifier {
  val strategy = new LexerImpl.Strategy[Identifier] {

    def apply(stream: SourceCodeStream): Result[SourceCodeStream, Identifier] = {
      val re = """[a-zA-Z_][a-zA-Z0-9_]*""".r

      re findPrefixOf stream match {
        case Some(text) =>
          val (source, streamAfterToken) = stream.take(text)
          val token = Identifier(text, source)
          Success(token, Seq.empty, streamAfterToken)
        case None => NoMatch()
      }
    }
  }
}