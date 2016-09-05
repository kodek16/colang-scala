package colang.ast.parsed.expression

import colang.ast.parsed.{Function, Scope}
import colang.ast.raw.{expression => raw}
import colang.issues.{Issue, Issues, Terms}

/**
  * Represents a function call.
  * @param function called function
  * @param arguments function arguments
  */
case class FunctionCall(function: Function,
                        arguments: Seq[Expression],
                        rawNode: Option[raw.FunctionCall]) extends Expression {

  val type_ = function.returnType
}

object FunctionCall {
  def analyze(rawExpr: raw.FunctionCall)(implicit scope: Scope): (Expression, Seq[Issue]) = {
    val function = rawExpr.function
    val args = rawExpr.arguments.args

    val (parsedFunction, functionIssues) = Expression.analyze(scope, function)

    val argsResult = args map { Expression.analyze(scope, _) }
    val parsedArgs = argsResult map { _._1 }
    val argsIssues = argsResult flatMap { _._2 }

    parsedFunction match {
      case OverloadedFunctionReference(of, _) =>
        val (overloadOption, overloadingIssues) = of.resolveOverload(parsedArgs map { _.type_ }, Some(rawExpr.source))
        val result = overloadOption match {
          case Some(overload) => FunctionCall(overload, parsedArgs, Some(rawExpr))
          case None => InvalidExpression()
        }
        (result, functionIssues ++ argsIssues ++ overloadingIssues)

      case FunctionReference(f, _) if f.canBeAppliedTo(parsedArgs map { _.type_ }) =>
        (FunctionCall(f, parsedArgs, Some(rawExpr)), functionIssues ++ argsIssues)

      case FunctionReference(f, _) =>
        val argTypeNames = parsedArgs map { _.type_.qualifiedName }
        val issue = Issues.InvalidCallArguments(rawExpr.source, (Terms.Function, argTypeNames))
        (InvalidExpression(), functionIssues ++ argsIssues :+ issue)

      case _ =>
        val issue = Issues.ExpressionIsNotCallable(function.source, ())
        (InvalidExpression(), functionIssues ++ argsIssues :+ issue)
    }
  }
}