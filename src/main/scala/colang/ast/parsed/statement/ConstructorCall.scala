package colang.ast.parsed.statement

import colang.ast.parsed.Constructor
import colang.ast.parsed.expression.Expression
import colang.ast.raw

/**
  * Represents a constructor call that initializes a variable.
  * @param constructor constructor to call
  * @param instance variable to initialize
  * @param arguments constructor arguments
  */
case class ConstructorCall(constructor: Constructor,
                           instance: Expression,
                           arguments: Seq[Expression],
                           rawNode: Option[raw.Node]) extends Statement
