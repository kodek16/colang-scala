package colang.ast.raw.statement

import colang.Strategy.Result
import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.TokenStream
import colang.ast.raw.ParserImpl
import colang.ast.raw.ParserImpl.{Present, SingleTokenStrategy}
import colang.ast.raw.expression.Expression
import colang.tokens.{LeftParen, RightParen, WhileKeyword}

/**
  * Represents a 'while' loop statement.
  * @param keyword 'while keyword
  * @param leftParen opening (
  * @param condition loop condition
  * @param rightParen closing )
  * @param loop loop body
  */
case class WhileStatement(keyword: WhileKeyword,
                          leftParen: LeftParen,
                          condition: Expression,
                          rightParen: RightParen,
                          loop: Statement) extends Statement {

  def source = keyword.source + loop.source
}

object WhileStatement {
  val strategy: ParserImpl.Strategy[WhileStatement] = new ParserImpl.Strategy[WhileStatement] {

    def apply(stream: TokenStream): Result[TokenStream, WhileStatement] = {
      ParserImpl.parseGroup()
        .element(SingleTokenStrategy(classOf[WhileKeyword]), "'while' keyword",       stopIfAbsent = true)
        .element(SingleTokenStrategy(classOf[LeftParen]),    "opening '('")
        .element(Expression.strategy,                        "'while' loop condition")
        .element(SingleTokenStrategy(classOf[RightParen]),   "closing ')")
        .element(Statement.strategy,                         "'while' loop body")
        .parse(stream)
        .as[WhileKeyword, LeftParen, Expression, RightParen, Statement] match {

        //OK
        case (Present(keyword), Present(leftParen), Present(condition), Present(rightParen), Present(loop),
              issues, streamAfterLoop) =>
          val statement = WhileStatement(keyword, leftParen, condition, rightParen, loop)
          Success(statement, issues, streamAfterLoop)

        //Missing ( or )
        case (Present(keyword), leftParenOption @ _, Present(condition), rightParenOption @ _, Present(loop),
              issues, streamAfterLoop) =>
          val leftParen = leftParenOption match {
            case Present(lp) => lp
            case _ => LeftParen(keyword.source.after)
          }
          val rightParen = rightParenOption match {
            case Present(rp) => rp
            case _ => RightParen(condition.source.after)
          }

          val statement = WhileStatement(keyword, leftParen, condition, rightParen, loop)
          Success(statement, issues, streamAfterLoop)

        //Missing condition or body
        case (Present(keyword), _, _, _, _, issues, streamAfterLoop) =>
          Malformed(issues, streamAfterLoop)

        //Missing 'while'
        case _ => NoMatch()
      }
    }
  }
}