package colang.ast.parsed.expression

import colang.ast.parsed.Constructor
import colang.ast.raw.{expression => raw}

/**
  * Represents an object constructor call.
  * @param constructor constructor to call
  * @param arguments constructor arguments
  */
case class ConstructorCall(constructor: Constructor,
                           arguments: Seq[Expression],
                           rawNode: Option[raw.FunctionCall]) extends Expression {

  def type_ = constructor.type_
}
