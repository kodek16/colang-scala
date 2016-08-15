package colang.ast.parsed

import colang.Issue
import colang.ast.raw
import colang.tokens.NativeKeyword

object MethodDefinition {

  /**
    * Tries to add a method to a type, doesn't parse body.
    * @param container enclosing type
    * @param rawMeth raw method definition
    * @return (new method object (possibly detached), encountered issues)
    */
  def register(container: Type, rawMeth: raw.FunctionDefinition): (Method, Seq[Issue]) = {
    val (returnType, returnTypeIssues) = Type.resolve(container, rawMeth.returnType)

    val methodBody = new CodeBlock(new LocalScope(Some(container)))

    val paramsAnalysisResults = rawMeth.parameterList.params map { p =>
      val (paramType, paramTypeIssues) = Type.resolve(container, p.type_)
      (new Variable(p.name.value, Some(p.source), Some(methodBody.innerScope), paramType), paramTypeIssues)
    }

    val params = paramsAnalysisResults map { _._1 }
    val paramsIssues = paramsAnalysisResults flatMap { _._2 }

    val native = rawMeth.specifiers.has(classOf[NativeKeyword])

    val method = new Method(
      name = rawMeth.name.value,
      declarationSite = Some(rawMeth.prototypeSource),
      container = container,
      returnType = returnType,
      parameters = params,
      body = methodBody,
      native = native)

    (method, returnTypeIssues ++ paramsIssues ++ container.tryAddMethod(method))
  }
}
