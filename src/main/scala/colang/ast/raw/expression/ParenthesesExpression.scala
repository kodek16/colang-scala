package colang.ast.raw.expression

import colang.Strategy.Result
import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.TokenStream
import colang.ast.raw.ParserImpl
import colang.ast.raw.ParserImpl.{Absent, Invalid, Present, SingleTokenStrategy}
import colang.issues.Terms
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
      ParserImpl.parseGroup(Terms.Expression in Terms.Parentheses)
        .definingElement(SingleTokenStrategy(classOf[LeftParen]))
        .element(Expression.strategy, Terms.Expression)
        .element(SingleTokenStrategy(classOf[RightParen]), Terms.ClosingParen)
        .parse(stream)
        .as[LeftParen, Expression, RightParen] match {

        case (Present(leftParen), Present(expression), rightParenOption, issues, streamAfterExpression) =>
          val rightParen = rightParenOption.toOption match {
            case Some(rp) => rp
            case None =>
              RightParen(expression.source.after)
          }

          Success(ParenthesesExpression(leftParen, expression, rightParen), issues, streamAfterExpression)
        case (Present(leftParen), Invalid() | Absent(), Present(rightParen), issues, streamAfterExpression) =>
          Malformed(issues, streamAfterExpression)
        case _ => NoMatch()
      }
    }
  }
}