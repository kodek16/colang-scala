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

  private val numberFormat = NumberFormat.getInstance(Locale.US)

  /**
    * Matches simple fractional numbers without exponent.
    */
  val simpleStrategy = new LexerImpl.Strategy[DoubleLiteral] {
    def apply(stream: SourceCodeStream): Result[SourceCodeStream, DoubleLiteral] = {
      val re = """-?\d+\.\d+""".r

      re findPrefixOf stream match {
        case Some(text) =>
          val (source, streamAfterToken) = stream.take(text)
          val token = DoubleLiteral(numberFormat.parse(text).doubleValue(), source)
          Success(token, Seq.empty, streamAfterToken)
        case None => NoMatch()
      }
    }
  }

  /**
    * Matches fractional numbers in scientific notation (like 1.2e3)
    */
  val scientificStrategy = new LexerImpl.Strategy[DoubleLiteral] {
    def apply(stream: SourceCodeStream): Result[SourceCodeStream, DoubleLiteral] = {
      val re = """(-?\d+\.\d+)[eE](\+|\-|)(\d+(\.\d+)?)""".r("significand", "exp-sign", "exponent")

      re findPrefixMatchOf stream match {
        case Some(m) =>
          val (source, streamAfterToken) = stream.take(m.toString)

          val significand = numberFormat.parse(m.group("significand")).doubleValue()
          val expSign = m.group("exp-sign") match {
            case "-" => -1
            case _ => 1
          }
          val exponent = numberFormat.parse(m.group("exponent")).doubleValue()

          val value = significand * Math.pow(10, expSign * exponent)
          val token = DoubleLiteral(value, source)
          Success(token, Seq.empty, streamAfterToken)
        case None => NoMatch()
      }
    }
  }

  val strategy = StrategyUnion(
    scientificStrategy,
    simpleStrategy)
}