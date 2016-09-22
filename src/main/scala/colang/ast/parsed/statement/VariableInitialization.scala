package colang.ast.parsed.statement

import colang.ast.parsed.Variable
import colang.ast.parsed.expression.Expression
import colang.ast.raw

/**
  * Represents a variable initialization that happens when the variable comes into scope.
  * @param variable variable to initialize
  * @param value value to initialize the variable with
  */
case class VariableInitialization(variable: Variable,
                                  value: Expression,
                                  rawNode: Option[raw.Node]) extends Statement
