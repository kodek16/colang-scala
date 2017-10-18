package colang.tokens

import colang.Strategy.Result
import colang.Strategy.Result.{NoMatch, Matched}
import colang.issues.Issues
import colang.{SourceCode, SourceCodeStream, StrategyUnion}

/**
  * Represents a literal integer number that represents a value of type 'int'.
  * @param value numeric value
  */
case class IntLiteral(value: Int, source: SourceCode) extends Token

object IntLiteral {

  /**
    * Matches simple digit-only integer literals.
    */
  private val simpleStrategy = new LexerImpl.Strategy[IntLiteral] {
    def apply(stream: SourceCodeStream) : Result[SourceCodeStream, IntLiteral] = {
      val re = """-?\d+(?![\w\.])""".r

      re findPrefixOf stream match {
        case Some(text) =>
          val (source, streamAfterToken) = stream.take(text)

          val bigValue = BigInt(text)

          if (bigValue >= Int.MinValue && bigValue <= Int.MaxValue) {
            Matched(
              IntLiteral(bigValue.toInt, source),
              issues = Seq.empty,
              streamAfterToken)

          } else {
            val issue = if (bigValue > 0) {
              Issues.NumericLiteralTooBig(source, "int")
            } else {
              Issues.NumericLiteralTooSmall(source, "int")
            }

            Matched(IntLiteral(0, source), Seq(issue), streamAfterToken)
          }

        case None => NoMatch()
      }
    }
  }

  /**
    * Matches invalid literals of form '1e5.4'.
    */
  private val malformedScientificStrategy = new LexerImpl.Strategy[IntLiteral] {
    def apply(stream: SourceCodeStream): Result[SourceCodeStream, IntLiteral] = {
      val re = """(-?\d+)([eE]\d+\.\d+|[eE]-\d+(\.\d+)?)(?![\w\.])""".r

      re findPrefixMatchOf stream match {
        case Some(m) =>
          val (source, streamAfterToken) = stream.take(m.toString)

          val doubleLiteral = m.group(1) + ".0" + m.group(2)
          val issue = Issues.IntegerLiteralWithNonNaturalExponent(
            source, doubleLiteral)

          Matched(IntLiteral(0, source), Seq(issue), streamAfterToken)
        case None => NoMatch()
      }
    }
  }

  /**
    * Matches integers in scientific notation (like 1e6).
    */
  private val scientificStrategy = new LexerImpl.Strategy[IntLiteral] {
    def apply(stream: SourceCodeStream): Result[SourceCodeStream, IntLiteral] = {
      val re = """(-?\d+)[eE](\d+)(?![\w\.])""".r

      re findPrefixMatchOf stream match {
        case Some(m) =>
          val (source, streamAfterToken) = stream.take(m.toString)

          val significand = BigInt(m.group(1))
          val exponent = BigInt(m.group(2))

          // Exponents greater than 20 are guaranteed to overflow, so we don't
          // calculate the actual value.
          if (exponent < 20) {
            val bigValue = significand * (BigInt(10) pow exponent.toInt)

            if (bigValue >= Int.MinValue && bigValue <= Int.MaxValue) {
              Matched(
                IntLiteral(bigValue.toInt, source),
                issues = Seq.empty,
                streamAfterToken)
            } else if (bigValue > 0) {
              Matched(
                IntLiteral(0, source),
                issues = Seq(Issues.NumericLiteralTooBig(source, "int")),
                streamAfterToken)
            } else {
              Matched(
                IntLiteral(0, source),
                issues = Seq(Issues.NumericLiteralTooSmall(source, "int")),
                streamAfterToken)
            }

          } else {
            Matched(
              IntLiteral(0, source),
              issues = Seq(Issues.NumericLiteralTooBig(source, "int")),
              streamAfterToken)
          }

        case None => NoMatch()
      }
    }
  }

  val strategy = StrategyUnion(
    malformedScientificStrategy,
    scientificStrategy,
    simpleStrategy)
}
