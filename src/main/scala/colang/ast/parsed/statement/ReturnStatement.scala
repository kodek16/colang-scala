package colang.ast.parsed.statement

import colang.ast.parsed.expression.Expression
import colang.ast.raw.{statement => raw}

/**
  * Represents a function return statement
  * @param returnValue value to be returned
  */
case class ReturnStatement(returnValue: Expression,
                           rawNode: Option[raw.ReturnStatement]) extends Statement
