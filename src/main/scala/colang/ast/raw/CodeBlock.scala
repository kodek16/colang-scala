package colang.ast.raw

import colang.Strategy.Result
import colang.Strategy.Result.{NoMatch, Matched}
import colang._
import colang.ast.raw.statement.Statement
import colang.issues.Terms
import colang.tokens.{LeftBrace, RightBrace, Semicolon}

/**
  * Represents a code block: statements enclosed in braces.
  * @param leftBrace opening brace
  * @param statements statements
  * @param rightBrace closing brace
  */
case class CodeBlock(leftBrace: LeftBrace, statements: Seq[Statement], rightBrace: RightBrace) extends Statement {
  def source = leftBrace.source + rightBrace.source
}

object CodeBlock {
  val strategy: ParserImpl.Strategy[CodeBlock] = new ParserImpl.Strategy[CodeBlock] {

    def apply(stream: TokenStream): Result[TokenStream, CodeBlock] = {
      ParserImpl.parseEnclosedSequence(
        stream = stream,
        sequenceDescription = Terms.Block of Terms.Code,
        elementStrategy = Statement.strategy,
        elementDescription = Terms.Statement,
        openingElement = classOf[LeftBrace],
        closingElement = classOf[RightBrace],
        closingElementDescription = Terms.ClosingBrace,
        recoveryStopHints = Seq(classOf[Semicolon])
      ) match {
        case Some((leftBrace, elements, rightBraceOption, issues, streamAfterBlock)) =>
          val rightBrace = rightBraceOption match {
            case Some(rb) => rb
            case None =>
              val previousSource = if (elements.nonEmpty) elements.last.source else leftBrace.source
              RightBrace(previousSource.after)
          }

          Matched(CodeBlock(leftBrace, elements, rightBrace), issues, streamAfterBlock)
        case None =>
          NoMatch()
      }
    }
  }
}
