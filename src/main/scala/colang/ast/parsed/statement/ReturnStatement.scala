package colang.ast.parsed.statement

import colang.ast.parsed.expression.Expression
import colang.ast.raw.{statement => raw}

/**
  * Represents a function return statement, either with a return value or without it.
  * @param returnValue optional value to be returned
  */
case class ReturnStatement(returnValue: Option[Expression],
                           rawNode: Option[raw.ReturnStatement]) extends Statement
