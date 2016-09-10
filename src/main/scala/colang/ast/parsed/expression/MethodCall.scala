package colang.ast.parsed.expression

import colang.ast.parsed.{Method, ReferenceType, Type}
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

object MethodCall {

  /**
    * Tries to construct a method call from given arguments, dereferencing instance if necessary.
    * @param instance call instance
    * @param methodName method name
    * @param arguments call arguments
    * @return Some(MethodCall) if successful
    */
  def tryConstruct(instance: Expression,
                   methodName: String,
                   arguments: Seq[Expression],
                   rawNode: Option[raw.Node]): Option[MethodCall] = {

    instance.type_ resolveMethod methodName match {
      case Some(m) if m.canBeAppliedTo(arguments map { _.type_ }) =>
        val operatorArgs = Type.performImplicitConversions(arguments, m.parameters map { _.type_ })
        Some(MethodCall(m, instance, operatorArgs, rawNode))

      case _ =>
        instance.type_ match {
          case rt: ReferenceType =>
            tryConstruct(ImplicitDereferencing(instance), methodName, arguments, rawNode)
          case _ => None
        }
    }
  }
}