package colang.ast.parsed.expression

import colang.ast.parsed.ReferenceType

/**
  * Represents an implicit type conversion from a reference type to its underlying plain type.
  * @param expression expression to convert
  */
case class ImplicitDereferencing(expression: Expression) extends Expression {
  def type_ = expression.type_.asInstanceOf[ReferenceType].referenced
  def rawNode = None
}
