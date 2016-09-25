package colang.ast.parsed.routines

import colang.ast.parsed.{CodeBlock, Function, LocalContext, LocalScope, Scope, Type, Variable}
import colang.ast.raw
import colang.issues.{Issue, Issues, Terms}
import colang.tokens.NativeKeyword

private[routines] object RegisterFunctions {

  /**
    * "Registers" given functions in the given scope, doesn't parse bodies.
    * @param scope enclosing scope (root namespace for global functions, container type for static functions)
    * @param funcDefs function definitions
    * @return (new functions, encountered issues)
    */
  def registerFunctions(scope: Scope, funcDefs: Seq[raw.FunctionDefinition]): (Seq[Function], Seq[Issue]) = {
    val result = funcDefs map { funcDef =>
      val (returnType, returnTypeIssues) = Type.resolve(funcDef.returnType)(scope)

      val refMarkerIssues = funcDef.referenceMarker.toSeq map { marker =>
        Issues.ReferenceMarkerInFunctionDefinition(marker.source, ())
      }

      val localContext = LocalContext(
        applicableKind = Terms.Function,
        expectedReturnType = Some(returnType))

      val functionBody = new CodeBlock(new LocalScope(Some(scope)), localContext, funcDef.body)

      val paramsResult = funcDef.parameterList.params map { rawParam =>
        val (paramType, paramTypeIssues) = Type.resolve(rawParam.type_)(scope)
        val param = Variable(
          name = rawParam.name.value,
          scope = Some(functionBody.innerScope),
          type_ = paramType,
          definition = Some(rawParam))

        (param, paramTypeIssues)
      }

      val params = paramsResult map { _._1 }
      val paramIssues = paramsResult flatMap { _._2 }

      val function = new Function(
        name = funcDef.name.value,
        scope = Some(scope),
        returnType = returnType,
        parameters = params,
        body = functionBody,
        definition = Some(funcDef),
        native = funcDef.specifiers.has(classOf[NativeKeyword]))

      val functionIssues = scope.tryAdd(function)
      (function, returnTypeIssues ++ refMarkerIssues ++ paramIssues ++ functionIssues)
    }

    val functions = result map { _._1 }
    val issues = result flatMap { _._2 }
    (functions, issues)
  }
}
