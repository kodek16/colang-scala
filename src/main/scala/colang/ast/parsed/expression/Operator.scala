package colang.ast.parsed.expression

import colang.ast.parsed.Scope
import colang.ast.raw.{expression => raw}
import colang.{Error, Issue, tokens}

/**
  * Operators calls are translated to method calls on the left operand.
  */
object Operator {

  def analyze(rawExpr: raw.InfixOperator)(implicit scope: Scope): (Expression, Seq[Issue]) = {
    val (rawLhs, operator, rawRhs) = (rawExpr.lhs, rawExpr.operator, rawExpr.rhs)

    val methodName = operator match {
      case tokens.Multiply(_) => "times"
      case tokens.Divide(_) => "div"
      case tokens.Plus(_) => "plus"
      case tokens.Minus(_) => "minus"
      case tokens.Less(_) => "lessThan"
      case tokens.Greater(_) => "greaterThan"
      case tokens.LessOrEquals(_) => "lessOrEquals"
      case tokens.GreaterOrEquals(_) => "greaterOrEquals"
      case tokens.Equals(_) => "equals"
      case tokens.NotEquals(_) => "notEquals"
      case tokens.LogicalAnd(_) => "and"
      case tokens.LogicalOr(_) => "or"
      case tokens.Assign(_) => "assign"
    }

    val (lhs, lhsIssues) = Expression.analyze(scope, rawLhs)
    val (rhs, rhsIssues) = Expression.analyze(scope, rawRhs)

    lhs.type_ resolveMethod methodName match {
      case Some(m) if m.canBeAppliedTo(Seq(rhs.type_)) =>
        (MethodCall(m, lhs, Seq(rhs)), lhsIssues ++ rhsIssues)
      case _ =>
        val (lType, rType) = (lhs.type_.qualifiedName, rhs.type_.qualifiedName)
        val issue = Error(rawExpr.source, s"operator '$methodName' is not defined for types '$lType' and '$rType'")
        (InvalidExpression(), lhsIssues ++ rhsIssues :+ issue)
    }
  }

  def analyze(rawExpr: raw.PrefixOperator)(implicit scope: Scope): (Expression, Seq[Issue]) = {
    val methodName = rawExpr.operator match {
      case tokens.LogicalNot(_) => "not"
      case tokens.Minus(_) => "unaryMinus"
    }

    val (expr, exprIssues) = Expression.analyze(scope, rawExpr.expression)

    expr.type_ resolveMethod methodName match {
      case Some(m) if m.canBeAppliedTo(Seq.empty) =>
        (MethodCall(m, expr, Seq.empty), exprIssues)
      case _ =>
        val exprType = expr.type_.qualifiedName
        val issue = Error(rawExpr.source, s"operator '$methodName' is not defined for type '$exprType'")
        (InvalidExpression(), exprIssues :+ issue)
    }
  }
}
