package colang.ast.raw.expression

import colang.Strategy.Result
import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.ast.raw.ParserImpl
import colang.ast.raw.ParserImpl.{Absent, Invalid, Present, SingleTokenStrategy}
import colang.tokens.{LogicalNot, Minus}
import colang.{StrategyUnion, TokenStream, tokens}

/**
  * Represents an unary prefix operator expression.
  * @param operator prefix operator
  * @param expression expression the operator is applied to
  */
case class PrefixOperator(operator: tokens.PrefixOperator, expression: Expression) extends Expression {
  def source = operator.source + expression.source
}

object PrefixOperator {
  val strategy = new ParserImpl.Strategy[PrefixOperator] {

    private val operatorStrategy = StrategyUnion(
      SingleTokenStrategy(classOf[LogicalNot]),
      SingleTokenStrategy(classOf[Minus]))

    def apply(stream: TokenStream): Result[TokenStream, PrefixOperator] = {
      ParserImpl.parseGroup()
        .element(operatorStrategy,           "prefix operator",                 stopIfAbsent = true)
        .element(Expression.primaryStrategy, "expression after prefix operator")
        .parse(stream)
        .as[tokens.PrefixOperator, Expression] match {

        case (Present(operator), Present(expression), issues, streamAfterExpression) =>
          Success(PrefixOperator(operator, expression), issues, streamAfterExpression)
        case (Present(operator), Invalid() | Absent(), issues, streamAfterExpression) =>
          Malformed(issues, streamAfterExpression)
        case _ => NoMatch()
      }
    }
  }
}