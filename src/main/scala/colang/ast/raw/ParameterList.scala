package colang.ast.raw

import colang.Strategy.Result
import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.ast.raw.ParserImpl.{Absent, Invalid, Present, SingleTokenStrategy}
import colang.tokens.{Comma, Identifier, LeftParen, RightParen}
import colang.{SourceCode, TokenStream}

/**
  * Represents a function parameter.
  * @param type_ parameter type
  * @param name parameter name
  */
case class FunctionParameter(type_ : Type, name: Identifier) extends Node {
  def source: SourceCode = type_.source + name.source
}

object FunctionParameter {
  val strategy = new ParserImpl.Strategy[FunctionParameter] {

    def apply(stream: TokenStream): Result[TokenStream, FunctionParameter] = {
      ParserImpl.parseGroup()
        .element(Type.strategy,                             "parameter type", stopIfAbsent = true)
        .element(SingleTokenStrategy(classOf[Identifier]),  "parameter name")
        .parse(stream)
        .as[Type, Identifier] match {

        case (Present(type_), Present(name), issues, streamAfterParam) =>
          Success(FunctionParameter(type_, name), issues, streamAfterParam)
        case (Present(_) | Invalid(), Invalid() | Absent(), issues, streamAfterParam) =>
          Malformed(issues, streamAfterParam)
        case _ => NoMatch()
      }
    }
  }
}

/**
  * Represents a function parameter list.
  * @param leftParen opening parentheses
  * @param params function parameters
  * @param rightParen closing parentheses
  */
case class ParameterList(leftParen: LeftParen, params: Seq[FunctionParameter], rightParen: RightParen) extends Node {
  def source: SourceCode = leftParen.source + rightParen.source
}

object ParameterList {
  val strategy = new ParserImpl.Strategy[ParameterList] {

    def apply(stream: TokenStream): Result[TokenStream, ParameterList] = {
      ParserImpl.parseEnclosedSequence(
        stream = stream,
        sequenceDescription = "parameter list",
        elementStrategy = FunctionParameter.strategy,
        elementDescription = "parameter",
        openingElement = classOf[LeftParen],
        openingElementDescription = "opening '('",
        closingElement = classOf[RightParen],
        closingElementDescription = "closing ')'",
        mandatorySeparator = Some(classOf[Comma]),
        separatorDescription = "comma"
      ) match {
        case Some((leftParen, params, rightParenOption, issues, streamAfterParams)) =>
          val rightParen = rightParenOption match {
            case Some(rp) => rp
            case None =>
              val previousSource = if (params.nonEmpty) params.last.source else leftParen.source
              RightParen(previousSource.after)
          }

          Success(ParameterList(leftParen, params, rightParen), issues, streamAfterParams)
        case _ => NoMatch()
      }
    }
  }
}