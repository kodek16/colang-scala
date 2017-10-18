package colang.ast.raw.statement

import colang.Strategy.Result
import colang.Strategy.Result.{NoMatch, Matched}
import colang.TokenStream
import colang.ast.raw.ParserImpl
import colang.ast.raw.ParserImpl.{Present, SingleTokenStrategy}
import colang.ast.raw.expression.Expression
import colang.tokens.ReturnKeyword

/**
  * Represents a function 'return' statement, either with a value or without it.
  * @param keyword 'return' keyword
  * @param expression optional value to return
  */
case class ReturnStatement(keyword: ReturnKeyword, expression: Option[Expression]) extends Statement {
  def source = keyword.source + expression.getOrElse(keyword).source
}

object ReturnStatement {
  val strategy: ParserImpl.Strategy[ReturnStatement] = new ParserImpl.Strategy[ReturnStatement] {

    def apply(stream: TokenStream): Result[TokenStream, ReturnStatement] = {
      ParserImpl.parseGroup()
        .definingElement(SingleTokenStrategy(classOf[ReturnKeyword]))
        .lineContinuation()
        .optionalElement(Expression.strategy)
        .parse(stream)
        .as[ReturnKeyword, Expression] match {

        case (Present(keyword), Present(expression), issues, streamAfterStatement) =>
          val statement = ReturnStatement(keyword, Some(expression))
          Matched(statement, issues, streamAfterStatement)

        case (Present(keyword), _, issues, streamAfterStatement) =>
          val statement = ReturnStatement(keyword, None)
          Matched(statement, issues, streamAfterStatement)

        case _ => NoMatch()
      }
    }
  }
}
