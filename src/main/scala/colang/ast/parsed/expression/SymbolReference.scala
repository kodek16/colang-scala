package colang.ast.parsed.expression

import colang.ast.parsed.{Function, OverloadedFunction, ReferenceVariable, Scope, Symbol, Variable}
import colang.ast.raw.{expression => raw}
import colang.issues.{Issue, Issues}

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

object VariableReference {

  /**
    * A factory method for instantiating the correct Expression class.
    */
  def apply(variable: Variable, rawNode: Option[raw.SymbolReference]): Expression = {
    variable match {
      case rv: ReferenceVariable => new ReferenceVariableReference(rv, rawNode)
      case v: Variable => new VariableReference(v, rawNode)
    }
  }
}

/**
  * Represents a variable reference.
  * @param variable referenced variable
  */
class VariableReference private (val variable: Variable,
                                 val rawNode: Option[raw.SymbolReference]) extends Expression {
  val type_ = variable.type_.reference
}

/**
  * Represents a "reference variable" reference.
  * @param variable referenced reference variable
  */
class ReferenceVariableReference(val variable: ReferenceVariable,
                                 val rawNode: Option[raw.SymbolReference]) extends Expression {
  val type_ = variable.type_
}

object SymbolReference {
  def analyze(rawExpr: raw.SymbolReference)(implicit scope: Scope): (Expression, Seq[Issue]) = {

    scope.resolve(rawExpr.name.value) match {
      case Some(v: Variable) => (VariableReference(v, Some(rawExpr)), Seq.empty)
      case Some(f: Function) => (FunctionReference(f, Some(rawExpr)), Seq.empty)
      case Some(of: OverloadedFunction) => (OverloadedFunctionReference(of, Some(rawExpr)), Seq.empty)

      case Some(s: Symbol) =>
        val issue = Issues.InvalidReferenceAsExpression(rawExpr.source, s.description)
        (InvalidExpression(), Seq(issue))

      case None =>
        val issue = Issues.UnknownName(rawExpr.source, ())
        (InvalidExpression(), Seq(issue))
    }
  }
}
