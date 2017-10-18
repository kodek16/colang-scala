package colang.ast.raw.expression

import colang.Strategy.Result
import colang.Strategy.Result.{Skipped, NoMatch, Matched}
import colang.ast.raw.ParserImpl
import colang.ast.raw.ParserImpl.{Present, SingleTokenStrategy}
import colang.issues.Terms
import colang.tokens.{Dot, Identifier}
import colang.{SourceCode, TokenStream}

/**
  * Represents an object member access expression.
  * @param instance object
  * @param memberName member name
  */
case class MemberAccess(instance: Expression, memberName: Identifier) extends Expression {
  def source = instance.source + memberName.source
}

object MemberAccess {
  val strategy = new ParserImpl.Strategy[PostfixOperator] {

    def apply(stream: TokenStream): Result[TokenStream, PostfixOperator] = {
      ParserImpl.parseGroup()
        .definingElement(SingleTokenStrategy(classOf[Dot]))
        .element(ParserImpl.identifierStrategy, Terms.Name of Terms.Member)
        .parse(stream)
        .as[Dot, Identifier] match {

        case (Present(dot), Present(memberName), issues, streamAfterExpression) =>
          val postfixOp = new PostfixOperator {
            def apply: (Expression) => Expression = { expr => MemberAccess(expr, memberName) }
            def source: SourceCode = dot.source + memberName.source
          }
          Matched(postfixOp, issues, streamAfterExpression)

        case (Present(dot), _, issues, streamAfterExpression) =>
          Skipped(issues, streamAfterExpression)

        case _ => NoMatch()
      }
    }
  }
}
