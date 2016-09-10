package colang.ast.parsed.expression

import colang.ast.parsed.{ReferenceType, Scope, Type}
import colang.ast.raw.{expression => raw}
import colang.issues.{Issue, Issues}
import colang.tokens

/**
  * Operators calls are translated to method calls on the left operand.
  */
object Operator {

  def analyze(rawExpr: raw.InfixOperator)(implicit scope: Scope): (Expression, Seq[Issue]) = {
    val (rawLhs, operator, rawRhs) = (rawExpr.lhs, rawExpr.operator, rawExpr.rhs)

    val methodName = operator match {
      case tokens.Multiply(_) => "times"
      case tokens.Divide(_) => "div"
      case tokens.Mod(_) => "mod"
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

    MethodCall.tryConstruct(lhs, methodName, Seq(rhs), Some(rawExpr)) match {
      case Some(methodCall) => (methodCall, lhsIssues ++ rhsIssues)

      case _ =>
        val argTypes = Seq(lhs.type_.qualifiedName, rhs.type_.qualifiedName)
        val issue = Issues.UndefinedOperator(rawExpr.source, (operator.name, argTypes))
        (InvalidExpression(), lhsIssues ++ rhsIssues :+ issue)
    }
  }

  def analyze(rawExpr: raw.PrefixOperator)(implicit scope: Scope): (Expression, Seq[Issue]) = {
    val methodName = rawExpr.operator match {
      case tokens.LogicalNot(_) => "not"
      case tokens.Minus(_) => "unaryMinus"
    }

    val (expr, exprIssues) = Expression.analyze(scope, rawExpr.expression)

    MethodCall.tryConstruct(expr, methodName, Seq.empty, Some(rawExpr)) match {
      case Some(methodCall) => (methodCall, exprIssues)

      case _ =>
        val argTypes = Seq(expr.type_.qualifiedName)
        val issue = Issues.UndefinedOperator(rawExpr.source, (rawExpr.operator.name, argTypes))
        (InvalidExpression(), exprIssues :+ issue)
    }
  }
}
