package colang.ast.parsed.expression

import colang.ast.parsed._
import colang.ast.raw.{expression => raw}
import colang.issues.{Issue, Issues}
import colang.tokens

/**
  * Operators calls are translated to method calls on the left operand.
  */
object Operator {

  def analyze(rawExpr: raw.InfixOperator)(implicit scope: Scope, localContext: LocalContext): (Expression, Seq[Issue]) = {
    rawExpr.operator match {
      case _: tokens.AsKeyword => analyzeCast(rawExpr)
      case _ => analyzeOverloadableOperator(rawExpr)
    }
  }

  private def analyzeCast(rawCastExpr: raw.InfixOperator)
                         (implicit scope: Scope, localContext: LocalContext): (Expression, Seq[Issue]) = {

    val (rawExpr, rawTargetTypeExpr) = (rawCastExpr.lhs, rawCastExpr.rhs)

    val (expr, exprIssues) = Expression.analyze(rawExpr)
    val (targetTypeExpr, targetTypeIssues) = Expression.analyze(rawTargetTypeExpr)

    targetTypeExpr match {
      case TypeReference(targetType, _) =>
        if (expr.type_ isImplicitlyConvertibleTo targetType) {
          (Type.performImplicitConversion(expr, targetType), exprIssues ++ targetTypeIssues)

        } else {
          val conversionFunctionOption = targetType.resolve("from") match {
            case Some(of: OverloadedFunction) => of.resolveOverload(Seq(expr.type_), None)._1
            case Some(f: Function) if f canBeAppliedTo Seq(expr.type_) => Some(f)
            case _ => None
          }

          conversionFunctionOption match {
            case Some(conversionFunction) if conversionFunction.returnType == targetType =>
              val convertedExpr = Type.performImplicitConversion(expr, conversionFunction.parameters.head.type_)
              (FunctionCall(conversionFunction, Seq(convertedExpr), Some(rawCastExpr)), exprIssues ++ targetTypeIssues)

            case Some(conversionFunction) =>
              val issue = Issues.InvalidConversionFunctionReturnType(rawCastExpr.source, targetType.qualifiedName)
              (InvalidExpression(), exprIssues ++ targetTypeIssues :+ issue)

            case None =>
              val issue = Issues.NoTypeConversionFunction(rawCastExpr.source,
                (expr.type_.qualifiedName, targetType.qualifiedName))
              (InvalidExpression(), exprIssues ++ targetTypeIssues :+ issue)
          }
        }

      case _ =>
        val issue = Issues.NonTypeExpressionAsCastTarget(rawTargetTypeExpr.source, ())
        (InvalidExpression(), exprIssues ++ targetTypeIssues :+ issue)
    }
  }

  private def analyzeOverloadableOperator(rawExpr: raw.InfixOperator)
                                         (implicit scope: Scope, localContext: LocalContext): (Expression, Seq[Issue]) = {

    val (rawLhs, operator, rawRhs) = (rawExpr.lhs, rawExpr.operator, rawExpr.rhs)

    val methodName = operator match {
      case tokens.Multiply(_) => "times"
      case tokens.Divide(_) => "div"
      case tokens.Mod(_) => "mod"
      case tokens.Pow(_) => "pow"
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

    val (lhs, lhsIssues) = Expression.analyze(rawLhs)
    val (rhs, rhsIssues) = Expression.analyze(rawRhs)

    MethodCall.tryConstruct(lhs, methodName, Seq(rhs), Some(rawExpr)) match {
      case Some(methodCall) => (methodCall, lhsIssues ++ rhsIssues)

      case _ =>
        val argTypes = Seq(lhs.type_.qualifiedName, rhs.type_.qualifiedName)
        val issue = Issues.UndefinedOperator(rawExpr.source, (operator.name, argTypes))
        (InvalidExpression(), lhsIssues ++ rhsIssues :+ issue)
    }
  }

  def analyze(rawExpr: raw.PrefixOperator)(implicit scope: Scope, localContext: LocalContext): (Expression, Seq[Issue]) = {
    val methodName = rawExpr.operator match {
      case tokens.LogicalNot(_) => "not"
      case tokens.Minus(_) => "unaryMinus"
    }

    val (expr, exprIssues) = Expression.analyze(rawExpr.expression)

    MethodCall.tryConstruct(expr, methodName, Seq.empty, Some(rawExpr)) match {
      case Some(methodCall) => (methodCall, exprIssues)

      case _ =>
        val argTypes = Seq(expr.type_.qualifiedName)
        val issue = Issues.UndefinedOperator(rawExpr.source, (rawExpr.operator.name, argTypes))
        (InvalidExpression(), exprIssues :+ issue)
    }
  }
}
