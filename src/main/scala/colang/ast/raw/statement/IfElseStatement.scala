package colang.ast.raw.statement

import colang.Strategy.Result
import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.TokenStream
import colang.ast.raw.ParserImpl
import colang.ast.raw.ParserImpl.{Absent, Invalid, Present, SingleTokenStrategy}
import colang.issues.Terms
import colang.tokens.ElseKeyword

/**
  * Represents a conditional statement with an 'else' branch ('if (cond) stmt1 else stmt2')
  * @param ifStatement common 'if' statement part
  * @param elseKeyword 'else' keyword
  * @param elseBranch else branch
  */
case class IfElseStatement(ifStatement: IfStatement,
                           elseKeyword: ElseKeyword,
                           elseBranch: Statement) extends Statement {

  def source = ifStatement.source + elseBranch.source
}

object IfElseStatement {
  val strategy: ParserImpl.Strategy[IfElseStatement] = new ParserImpl.Strategy[IfElseStatement] {

    def apply(stream: TokenStream): Result[TokenStream, IfElseStatement] = {
      ParserImpl.parseGroup()
        .definingElement(IfStatement.strategy)
        .definingElement(SingleTokenStrategy(classOf[ElseKeyword]))
        .element(Statement.strategy, Terms.Branch("else"))
        .parse(stream)
        .as[IfStatement, ElseKeyword, Statement] match {

        case (Present(ifStatement), Present(elseKeyword), Present(elseBranch), issues, streamAfterStatement) =>
          val statement = IfElseStatement(ifStatement, elseKeyword, elseBranch)
          Success(statement, issues, streamAfterStatement)

        case (Present(ifStatement), Present(elseKeyword), Invalid() | Absent(), issues, streamAfterStatement) =>
          Malformed(issues, streamAfterStatement)

        case _ => NoMatch()
      }
    }
  }
}