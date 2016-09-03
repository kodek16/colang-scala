package colang.ast.raw.statement

import colang.Strategy.Result
import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.TokenStream
import colang.ast.raw.ParserImpl
import colang.ast.raw.ParserImpl.{Present, SingleTokenStrategy}
import colang.ast.raw.expression.Expression
import colang.issues.{Adjectives, Terms}
import colang.tokens.{IfKeyword, LeftParen, RightParen}

/**
  * Represents a simple conditional statement of form 'if (cond) stmt'
  * @param ifKeyword 'if' keyword
  * @param leftParen opening (
  * @param condition condition
  * @param rightParen closing )
  * @param ifBranch 'if' branch
  */
case class IfStatement(ifKeyword: IfKeyword,
                       leftParen: LeftParen,
                       condition: Expression,
                       rightParen: RightParen,
                       ifBranch: Statement) extends Statement {

  def source = ifKeyword.source + ifBranch.source
}

object IfStatement {
  val strategy: ParserImpl.Strategy[IfStatement] = new ParserImpl.Strategy[IfStatement] {

    def apply(stream: TokenStream): Result[TokenStream, IfStatement] = {
      ParserImpl.parseGroup(Terms.Statement("if"))
        .definingElement(SingleTokenStrategy(classOf[IfKeyword]))
        .element(SingleTokenStrategy(classOf[LeftParen]), Terms.OpeningParen)
        .element(Expression.strategy, Adjectives.Conditional applyTo Terms.Expression)
        .element(SingleTokenStrategy(classOf[RightParen]), Terms.ClosingParen)
        .element(Statement.strategy, Terms.Branch("if"))
        .parse(stream)
        .as[IfKeyword, LeftParen, Expression, RightParen, Statement] match {

        //Everything is OK
        case (Present(ifKeyword), Present(leftParen), Present(condition), Present(rightParen), Present(ifBranch),
              issues, streamAfterStatement) =>
          val statement = IfStatement(ifKeyword, leftParen, condition, rightParen, ifBranch)
          Success(statement, issues, streamAfterStatement)

        //Missing ( or )
        case (Present(ifKeyword), leftParenOption @ _, Present(condition), rightParenOption @ _, Present(ifBranch),
              issues, streamAfterStatement) =>
          val leftParen = leftParenOption match {
            case Present(lp) => lp
            case _ => LeftParen(ifKeyword.source.after)
          }
          val rightParen = rightParenOption match {
            case Present(rp) => rp
            case _ => RightParen(condition.source.after)
          }

          val statement = IfStatement(ifKeyword, leftParen, condition, rightParen, ifBranch)
          Success(statement, issues, streamAfterStatement)

        //Missing condition or body
        case (Present(ifKeyword), _, _, _, _, issues, streamAfterStatement) =>
          Malformed(issues, streamAfterStatement)

        //Missing 'if'
        case _ => NoMatch()
      }
    }
  }
}