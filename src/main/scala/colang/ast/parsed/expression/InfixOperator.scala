package colang.ast.parsed.expression

import colang.ast.parsed.Scope
import colang.{Error, Issue, tokens}
import colang.ast.raw.{expression => raw}

/**
  * Infix operators are translated to method calls on the left operand.
  */
object InfixOperator {
  def analyze(rawExpr: raw.InfixOperator)(implicit scope: Scope): (Expression, Seq[Issue]) = {
    val (rawLhs, operator, rawRhs) = (rawExpr.lhs, rawExpr.operator, rawExpr.rhs)

    val methodName = operator match {
      case tokens.Plus(_) => "plus"
      case tokens.Equals(_) => "equals"
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
}
