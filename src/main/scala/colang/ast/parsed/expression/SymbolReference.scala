package colang.ast.parsed.expression

import colang.ast.parsed.{Function, OverloadedFunction, ReferenceType, ReferenceVariable, Scope, Symbol, Type, Variable}
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
  def apply(variable: Variable, rawNode: Option[raw.SymbolReference]) = new VariableReference(variable, rawNode)
  def unapply(arg: VariableReference): Option[Variable] = Some(arg.variable)
}

object ReferenceVariableReference {

  /**
    * A special extractor for "reference variable" references
    */
  def unapply(arg: VariableReference): Option[(Variable, ReferenceType)] = arg match {
    case VariableReference(rv @ ReferenceVariable(_, rt)) => Some(rv, rt)
    case _ => None
  }
}

/**
  * Represents a variable reference.
  * Note that there is an additional ReferenceVariableReference extractor for cases when you care whether the referenced
  * variable has reference type.
  * @param variable referenced variable
  */
class VariableReference private (val variable: Variable,
                                 val rawNode: Option[raw.SymbolReference]) extends Expression {
  val type_ = variable.type_ match {
    case rt: ReferenceType => rt
    case t: Type => t.reference
  }
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
