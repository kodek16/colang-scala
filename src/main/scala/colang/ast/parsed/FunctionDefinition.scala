package colang.ast.parsed

import colang.{Error, Issue}
import colang.ast.raw
import colang.tokens.NativeKeyword

object FunctionDefinition {

  /**
    * Tries to add a function to the namespace, doesn't parse body.
    * @param namespace enclosing namespace
    * @param rawFunc raw function definition
    * @return (new function object (possibly detached), encountered issues)
    */
  def register(namespace: Namespace, rawFunc: raw.FunctionDefinition): (Function, Seq[Issue]) = {
    val (returnType, returnTypeIssues) = Type.resolve(namespace, rawFunc.returnType)

    val functionBody = new CodeBlock(new LocalScope(Some(namespace)))

    val paramsAnalysisResults = rawFunc.parameterList.params map { p =>
      val (paramType, paramTypeIssues) = Type.resolve(namespace, p.type_)
      (new Variable(p.name.value, Some(p.source), Some(functionBody.innerScope), paramType), paramTypeIssues)
    }

    val params = paramsAnalysisResults map { _._1 }
    val paramsIssues = paramsAnalysisResults flatMap { _._2 }

    val native = rawFunc.specifiers.has(classOf[NativeKeyword])

    val function = new Function(
      name = rawFunc.name.value,
      declarationSite = Some(rawFunc.prototypeSource),
      scope = Some(namespace),
      returnType = returnType,
      parameters = params,
      body = functionBody,
      native = native)

    (function, returnTypeIssues ++ paramsIssues ++ namespace.tryAdd(function))
  }

  /**
    * Analyzes function body.
    * @param function function object
    * @param rawFunc raw function definition
    * @return encountered issues
    */
  def analyzeBody(function: Function, rawFunc: raw.FunctionDefinition): Seq[Issue] = {
    val paramIssues = function.parameters flatMap { function.body.innerScope.tryAdd(_) }

    val bodyIssues = rawFunc.body match {
      case Some(body) =>
        val nativeIssues = if (function.native) {
          Some(Error(rawFunc.body.get.source, "native function can't be defined with a body"))
        } else None


        nativeIssues ++ function.body.addStatementsFromBlock(body)
      case None =>
        if (!function.native) {
          Seq(Error(rawFunc.prototypeSource, "missing function body"))
        } else Seq.empty
    }

    paramIssues ++ bodyIssues
  }
}
