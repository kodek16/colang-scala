package colang.ast.raw.expression

import colang.Strategy.Result
import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.TokenStream
import colang.ast.raw.ParserImpl
import colang.ast.raw.ParserImpl.SingleTokenStrategy
import colang.tokens.{LeftParen, RightParen}

/**
  * Represents an expression enclosed in parentheses.
  * @param leftParen left parenthesis
  * @param expression enclosed expression
  * @param rightParen right parenthesis
  */
case class ParenthesesExpression(leftParen: LeftParen,
                                 expression: Expression,
                                 rightParen: RightParen) extends Expression {
  def source = leftParen.source + rightParen.source
}

object ParenthesesExpression {
  val strategy = new ParserImpl.Strategy[ParenthesesExpression] {

    def apply(stream: TokenStream): Result[TokenStream, ParenthesesExpression] = {
      ParserImpl.parseGroup()
        .element(SingleTokenStrategy(classOf[LeftParen]),   "opening '('", stopIfAbsent = true)
        .element(Expression.strategy,                       "expression inside parentheses")
        .element(SingleTokenStrategy(classOf[RightParen]),  "closing ')'")
        .parse(stream)
        .as[LeftParen, Expression, RightParen] match {

        case (Some(leftParen), Some(expression), rightParenOption, issues, streamAfterExpression) =>
          val rightParen = rightParenOption match {
            case Some(rp) => rp
            case None =>
              RightParen(expression.source.after)
          }

          Success(ParenthesesExpression(leftParen, expression, rightParen), issues, streamAfterExpression)
        case (Some(leftParen), None, Some(rightParen), issues, streamAfterExpression) =>
          Malformed(issues, streamAfterExpression)
        case _ => NoMatch()
      }
    }
  }
}