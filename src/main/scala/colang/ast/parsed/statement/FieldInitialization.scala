package colang.ast.parsed.statement

import colang.ast.parsed.Field
import colang.ast.parsed.expression.Expression
import colang.ast.raw

/**
  * Represents a field initialization that happens before the object constructor invokation.
  * @param field field to initialize
  * @param value value to initialize field with
  */
case class FieldInitialization(field: Field,
                               value: Expression,
                               rawNode: Option[raw.Node]) extends Statement
