package colang.ast.raw.expression

import colang.ast.raw.ParserImpl
import colang.{MappedStrategy, SourceCode}

/**
  * Represents a function call expression.
  * @param function function to call
  * @param arguments function arguments
  */
case class FunctionCall(function: Expression, arguments: ArgumentList) extends Expression {
  def source: SourceCode = function.source + arguments.source
}

object FunctionCall {
  val strategy: ParserImpl.Strategy[PostfixOperator] = MappedStrategy(
    ArgumentList.strategy,
    (arguments: ArgumentList) => new PostfixOperator {
      def apply = function => FunctionCall(function, arguments)
      def source: SourceCode = arguments.source
    })
}
