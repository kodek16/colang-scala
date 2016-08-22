package colang.ast.parsed.expression

import colang.ast.parsed.{Function, Namespace, Scope, Type, Variable}
import colang.ast.raw.{expression => raw}
import colang.{Error, Issue}

/**
  * Represents a function reference.
  * @param function referenced function
  */
case class FunctionReference(function: Function) extends Expression {
  val type_ = function.functionType
}

/**
  * Represents a variable reference.
  * @param variable referenced variable
  */
case class VariableReference(variable: Variable) extends Expression {
  val type_ = variable.type_
}

object SymbolReference {
  def analyze(rawExpr: raw.SymbolReference)(implicit scope: Scope): (Expression, Seq[Issue]) = {

    scope.resolve(rawExpr.name.value) match {
      case Some(v: Variable) => (VariableReference(v), Seq.empty)
      case Some(f: Function) => (FunctionReference(f), Seq.empty)

      case Some(s @ (_: Type | _: Namespace)) =>
        val issue = Error(rawExpr.source, s"expected an expression, but got ${s.description} reference")
        (InvalidExpression(), Seq(issue))

      case Some(_) => throw new RuntimeException("unhandled symbol type")
      case None =>
        val issue = Error(rawExpr.source, "there are no symbols with this name in the current scope")
        (InvalidExpression(), Seq(issue))
    }
  }
}
