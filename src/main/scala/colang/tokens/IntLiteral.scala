package colang.tokens

import colang.Strategy.Result
import colang.Strategy.Result.{NoMatch, Success}
import colang.{Error, SourceCode, SourceCodeStream, StrategyUnion}

/**
  * Represents a literal integer number that corresponds to a value of type 'int'
  * @param value numeric value
  */
case class IntLiteral(value: Int, source: SourceCode) extends Token

object IntLiteral {

  /**
    * Matches simple digit-only integer literals.
    */
  val simpleStrategy = new LexerImpl.Strategy[IntLiteral] {
    def apply(stream: SourceCodeStream): Result[SourceCodeStream, IntLiteral] = {
      val re = """-?\d+(?![\w\.])""".r
      re findPrefixOf stream match {
        case Some(text) =>
          val (source, streamAfterToken) = stream.take(text)

          val bigValue = BigInt(text)
          if (bigValue >= Int.MinValue && bigValue <= Int.MaxValue) {
            Success(IntLiteral(bigValue.toInt, source), Seq.empty, streamAfterToken)
          } else {
            val relation = if (bigValue > 0) "big" else "small"
            val issue = Error(source, s"the literal value is too $relation for type 'int'")
            Success(IntLiteral(0, source), Seq(issue), streamAfterToken)
          }
        case None => NoMatch()
      }
    }
  }

  /**
    * Matches invalid literals of form '1e5.4'.
    */
  val malformedScientificStrategy = new LexerImpl.Strategy[IntLiteral] {
    def apply(stream: SourceCodeStream): Result[SourceCodeStream, IntLiteral] = {
      val re = """(-?\d+)([eE][-+]?\d*\.\d+)(?![\w\.])""".r
      re findPrefixMatchOf stream match {
        case Some(m) =>
          val (source, streamAfterToken) = stream.take(m.toString)

          val doubleLiteral = m.group(1) + ".0" + m.group(2)
          val issue = Error(source, "integer literals can't have non-natural exponents. To make this a 'double' type " +
            s"literal, rewrite it as '$doubleLiteral'")

          Success(IntLiteral(0, source), Seq(issue), streamAfterToken)
        case None => NoMatch()
      }
    }
  }

  /**
    * Matches integers in scientific notation (like 1e6).
    */
  val scientificStrategy = new LexerImpl.Strategy[IntLiteral] {
    def apply(stream: SourceCodeStream): Result[SourceCodeStream, IntLiteral] = {
      val re = """(-?\d+)[eE](\d+)(?![\w\.])""".r
      re findPrefixMatchOf stream match {
        case Some(m) =>
          val (source, streamAfterToken) = stream.take(m.toString)

          val significand = BigInt(m.group(1))
          val exponent = BigInt(m.group(2))

          val possibleErrorToken = IntLiteral(0, source)
          val relation = if (significand > 0) "big" else "small"
          val possibleError = Error(source, s"the literal value is too $relation for type 'int'")

          val (token, issues) = if (exponent < 20) {
            val bigValue = significand * (BigInt(10) pow exponent.toInt)

            if (bigValue >= Int.MinValue && bigValue <= Int.MaxValue) {
              (IntLiteral(bigValue.toInt, source), Seq.empty)
            } else (possibleErrorToken, Seq(possibleError))
          } else (possibleErrorToken, Seq(possibleError))

          Success(token, issues, streamAfterToken)
        case None => NoMatch()
      }
    }
  }

  val strategy = StrategyUnion(
    malformedScientificStrategy,
    scientificStrategy,
    simpleStrategy)
}
