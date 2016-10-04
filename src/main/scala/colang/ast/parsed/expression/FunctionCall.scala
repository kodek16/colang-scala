package colang.ast.parsed.expression

import colang.ast.parsed.{Function, LocalContext, Scope, Type}
import colang.ast.raw.{expression => raw}
import colang.issues.{Issue, Issues, Terms}

/**
  * Represents a function call.
  * @param function called function
  * @param arguments function arguments
  */
case class FunctionCall(function: Function,
                        arguments: Seq[Expression],
                        rawNode: Option[raw.Expression]) extends Expression {

  val type_ = function.returnType
}

object FunctionCall {

  // Note that method and constructor calls are also represented by raw.FunctionCall objects.
  // This function correctly handles them, creating appropriate MethodCall and ConstructorCall expressions.
  def analyze(rawExpr: raw.FunctionCall)(implicit scope: Scope, localContext: LocalContext): (Expression, Seq[Issue]) = {
    val function = rawExpr.function
    val args = rawExpr.arguments.args

    val (parsedFunction, functionIssues) = Expression.analyze(function)

    val argsResult = args map Expression.analyze
    val parsedArgs = argsResult map { _._1 }
    val argsIssues = argsResult flatMap { _._2 }

    parsedFunction match {

      // Function calls:
      case OverloadedFunctionReference(of, _) =>
        val (overloadOption, overloadingIssues) = of.resolveOverload(parsedArgs map { _.type_ }, Some(rawExpr.source))
        val result = overloadOption match {
          case Some(overload) =>
            val functionArgs = Type.performImplicitConversions(parsedArgs, overload.parameters map { _.type_ })
            FunctionCall(overload, functionArgs, Some(rawExpr))

          case None => InvalidExpression()
        }
        (result, functionIssues ++ argsIssues ++ overloadingIssues)

      case FunctionReference(f, _) if f.canBeAppliedTo(parsedArgs map { _.type_ }) =>
        val functionArgs = Type.performImplicitConversions(parsedArgs, f.parameters map { _.type_ })
        (FunctionCall(f, functionArgs, Some(rawExpr)), functionIssues ++ argsIssues)

      case FunctionReference(f, _) =>
        val argTypeNames = parsedArgs map { _.type_.qualifiedName }
        val issue = Issues.InvalidCallArguments(rawExpr.source, (Terms.Function, argTypeNames))
        (InvalidExpression(), functionIssues ++ argsIssues :+ issue)

      // Method calls:
      case OverloadedMethodAccess(instance, om, _) =>
        val (overloadOption, overloadingIssues) = om.resolveOverload(parsedArgs map { _.type_ }, Some(rawExpr.source))
        val result = overloadOption match {
          case Some(overload) =>
            val methodArgs = Type.performImplicitConversions(parsedArgs, overload.parameters map { _.type_ })
            MethodCall(overload, instance, methodArgs, Some(rawExpr))

          case None => InvalidExpression()
        }
        (result, functionIssues ++ argsIssues ++ overloadingIssues)

      case MethodAccess(instance, m, _) if m.canBeAppliedTo(parsedArgs map { _.type_ }) =>
        val methodArgs = Type.performImplicitConversions(parsedArgs, m.parameters map { _.type_ })
        (MethodCall(m, instance, methodArgs, Some(rawExpr)), functionIssues ++ argsIssues)

      case MethodAccess(instance, m, _) =>
        val argTypeNames = parsedArgs map { _.type_.qualifiedName }
        val issue = Issues.InvalidCallArguments(rawExpr.source, (Terms.Method, argTypeNames))
        (InvalidExpression(), functionIssues ++ argsIssues :+ issue)

      // Constructor calls:
      case TypeReference(type_, _) =>
        val (constructorOption, overloadingIssues) = type_.resolveConstructor(parsedArgs map { _.type_ }, Some(rawExpr.source))
        val result = constructorOption match {
          case Some(constructor) =>
            val constructorArgs = Type.performImplicitConversions(parsedArgs, constructor.parameters map { _.type_ })
            ConstructorCall(constructor, constructorArgs, Some(rawExpr))

          case None => InvalidExpression()
        }
        (result, functionIssues ++ argsIssues ++ overloadingIssues)

      case _ =>
        val issue = Issues.ExpressionIsNotCallable(function.source, ())
        (InvalidExpression(), functionIssues ++ argsIssues :+ issue)
    }
  }
}
