package colang.ast.raw.statement

import colang.Strategy.Result
import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.ast.raw.ParserImpl.SingleTokenStrategy
import colang.ast.raw.expression.Expression
import colang.ast.raw.{CodeBlock, Node, ParserImpl}
import colang.tokens.Semicolon
import colang.{StrategyUnion, TokenStream}

/**
  * Represents a node that can be executed.
  */
trait Statement extends Node

object Statement {

  /**
    * A strategy for parsing semicolon. Like comment strategies, never produces any actual statements, instead used
    * Malformed with no issues for skipping semicolons.
    */
  val semicolonStrategy = new ParserImpl.Strategy[Statement] {
    private val tokenStrategy = SingleTokenStrategy(classOf[Semicolon])

    def apply(stream: TokenStream): Result[TokenStream, Statement] = {
      tokenStrategy(stream) match {
        case Success(_, _, streamAfterSemicolon) => Malformed(Seq.empty, streamAfterSemicolon)
        case Malformed(_, streamAfterSemicolon)  => Malformed(Seq.empty, streamAfterSemicolon)
        case NoMatch() => NoMatch()
      }
    }
  }

  /**
    * A strategy for parsing statements.
    */
  val strategy: ParserImpl.Strategy[Statement] = StrategyUnion(
    semicolonStrategy,
    IfElseStatement.strategy,
    IfStatement.strategy,
    WhileStatement.strategy,
    VariablesDefinition.strategy,
    Expression.strategy,
    CodeBlock.strategy)
}