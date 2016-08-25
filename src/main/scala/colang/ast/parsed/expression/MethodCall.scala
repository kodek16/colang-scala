package colang.ast.parsed.expression

import colang.ast.parsed.{Method, Type}
import colang.ast.raw

/**
  * Represents a method call.
  * @param method called method
  * @param instance bound object
  * @param arguments method arguments
  */
case class MethodCall(method: Method,
                      instance: Expression,
                      arguments: Seq[Expression],
                      rawNode: Option[raw.Node]) extends Expression {

  def type_ : Type = method.returnType
}
