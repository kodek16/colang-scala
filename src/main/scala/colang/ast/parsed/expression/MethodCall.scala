package colang.ast.parsed.expression

import colang.ast.parsed.{Method, Type}

/**
  * Represents a method call.
  * @param method called method
  * @param instance bound object
  * @param arguments method arguments
  */
case class MethodCall(method: Method, instance: Expression, arguments: Seq[Expression]) extends Expression {
  def type_ : Type = method.returnType
  val rawNode = None
}
