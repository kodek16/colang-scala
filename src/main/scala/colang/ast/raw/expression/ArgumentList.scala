package colang.ast.raw.expression

import colang.Strategy.Result
import colang.Strategy.Result.{NoMatch, Success}
import colang.ast.raw.{Node, ParserImpl}
import colang.issues.Terms
import colang.tokens.{Comma, LeftParen, RightParen}
import colang.{SourceCode, TokenStream}

/**
  * Represents an argument list in a function call.
  * @param leftParen left parenthesis
  * @param args arguments
  * @param rightParen right parenthesis
  */
case class ArgumentList(leftParen: LeftParen, args: Seq[Expression], rightParen: RightParen) extends Node {
  def source: SourceCode = leftParen.source + rightParen.source
}

object ArgumentList {
  val strategy = new ParserImpl.Strategy[ArgumentList] {

    def apply(stream: TokenStream): Result[TokenStream, ArgumentList] = {
      ParserImpl.parseEnclosedSequence(
        stream = stream,
        sequenceDescription = Terms.List of Terms.Arguments,
        elementStrategy = Expression.strategy,
        elementDescription = Terms.Argument,
        openingElement = classOf[LeftParen],
        closingElement = classOf[RightParen],
        closingElementDescription = Terms.ClosingParen,
        mandatorySeparator = Some(classOf[Comma]),
        separatorDescription = Some(Terms.Comma)
      ) match {
        case Some((leftParen, args, rightParenOption, issues, streamAfterNode)) =>
          val rightParen = rightParenOption match {
            case Some(rp) => rp
            case None =>
              val previousSource = if (args.nonEmpty) args.last.source else leftParen.source
              RightParen(previousSource.after)
          }

          Success(ArgumentList(leftParen, args, rightParen), issues, streamAfterNode)
        case _ => NoMatch()
      }
    }
  }
}
