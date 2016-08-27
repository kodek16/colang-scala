package colang.ast.parsed.expression

import colang.ast.parsed.{Function, Namespace, OverloadedFunction, Scope, Type, Variable}
import colang.ast.raw.{expression => raw}
import colang.{Error, Issue}

/**
  * Represents a function reference.
  * @param function referenced function
  */
case class FunctionReference(function: Function, rawNode: Option[raw.SymbolReference]) extends Expression {
  val type_ = function.functionType
}

/**
  * Represents an overloaded function reference.
  * @param overloadedFunction referenced overloaded function
  */
case class OverloadedFunctionReference(overloadedFunction: OverloadedFunction,
                                       rawNode: Option[raw.SymbolReference]) extends Expression {

  val type_ = overloadedFunction.scope.get.root.overloadedFunctionType
}

/**
  * Represents a variable reference.
  * @param variable referenced variable
  */
case class VariableReference(variable: Variable, rawNode: Option[raw.SymbolReference]) extends Expression {
  val type_ = variable.type_
}

object SymbolReference {
  def analyze(rawExpr: raw.SymbolReference)(implicit scope: Scope): (Expression, Seq[Issue]) = {

    scope.resolve(rawExpr.name.value) match {
      case Some(v: Variable) => (VariableReference(v, Some(rawExpr)), Seq.empty)
      case Some(f: Function) => (FunctionReference(f, Some(rawExpr)), Seq.empty)
      case Some(of: OverloadedFunction) => (OverloadedFunctionReference(of, Some(rawExpr)), Seq.empty)

      case Some(s @ (_: Type | _: Namespace)) =>
        val issue = Error(rawExpr.source, s"expected an expression, but got a ${s.description} reference")
        (InvalidExpression(), Seq(issue))

      case Some(_) => throw new RuntimeException("unhandled symbol type")
      case None =>
        val issue = Error(rawExpr.source, "there are no symbols with this name in the current scope")
        (InvalidExpression(), Seq(issue))
    }
  }
}
