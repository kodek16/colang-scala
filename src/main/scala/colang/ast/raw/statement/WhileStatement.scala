package colang.ast.raw.statement

import colang.Strategy.Result
import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.TokenStream
import colang.ast.raw.ParserImpl
import colang.ast.raw.ParserImpl.{Present, SingleTokenStrategy}
import colang.ast.raw.expression.Expression
import colang.issues.{Adjectives, Terms}
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
        .definingElement(SingleTokenStrategy(classOf[WhileKeyword]))
        .element(SingleTokenStrategy(classOf[LeftParen]), Terms.OpeningParen)
        .element(Expression.strategy, Adjectives.Conditional applyTo Terms.Expression)
        .element(SingleTokenStrategy(classOf[RightParen]), Terms.ClosingParen)
        .element(Statement.strategy, Terms.Body of Terms.Loop)
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
