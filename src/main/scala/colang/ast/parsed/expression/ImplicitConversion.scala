package colang.ast.parsed.expression

import colang.ast.parsed.Type

/**
  * Represents an implicit type conversion.
  * @param expression expression to convert
  * @param to target type
  */
case class ImplicitConversion(expression: Expression, to: Type) extends Expression {
  def type_ = to
  def rawNode = None
}
