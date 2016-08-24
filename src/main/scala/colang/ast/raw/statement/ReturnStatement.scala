package colang.ast.raw.statement

import colang.Strategy.Result
import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.TokenStream
import colang.ast.raw.ParserImpl
import colang.ast.raw.ParserImpl.{Present, SingleTokenStrategy}
import colang.ast.raw.expression.Expression
import colang.tokens.ReturnKeyword

/**
  * Represents a function 'return' statement.
  * @param keyword 'return' keyword
  * @param expression expression to return
  */
case class ReturnStatement(keyword: ReturnKeyword, expression: Expression) extends Statement {
  def source = keyword.source + expression.source
}

object ReturnStatement {
  val strategy: ParserImpl.Strategy[ReturnStatement] = new ParserImpl.Strategy[ReturnStatement] {

    def apply(stream: TokenStream): Result[TokenStream, ReturnStatement] = {
      ParserImpl.parseGroup()
        .element(SingleTokenStrategy(classOf[ReturnKeyword]), "'return' keyword", stopIfAbsent = true)
        .element(Expression.strategy,                         "return value")
        .parse(stream)
        .as[ReturnKeyword, Expression] match {

        case (Present(keyword), Present(expression), issues, streamAfterStatement) =>
          val statement = ReturnStatement(keyword, expression)
          Success(statement, issues, streamAfterStatement)

        case (Present(keyword), _, issues, streamAfterStatement) =>
          Malformed(issues, streamAfterStatement)

        case _ => NoMatch()
      }
    }
  }
}