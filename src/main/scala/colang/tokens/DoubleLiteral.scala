package colang.tokens

import java.text.NumberFormat
import java.util.Locale

import colang.Strategy.Result
import colang.Strategy.Result.{NoMatch, Success}
import colang._

/**
  * Represents a literal fractional number that corresponds to a value of type 'double'.
  * @param value numeric value
  */
case class DoubleLiteral(value: Double, source: SourceCode) extends Token

object DoubleLiteral {

  val strategy = new LexerImpl.Strategy[DoubleLiteral] {

    def apply(stream: SourceCodeStream): Result[SourceCodeStream, DoubleLiteral] = {
      val re = """\d+\.\d+""".r
      val numberFormat = NumberFormat.getInstance(Locale.US)

      re findPrefixOf stream match {
        case Some(text) =>
          val (source, streamAfterToken) = stream.take(text)
          val token = DoubleLiteral(numberFormat.parse(text).doubleValue(), source)
          Success(token, Seq.empty, streamAfterToken)
        case None => NoMatch()
      }
    }
  }
}