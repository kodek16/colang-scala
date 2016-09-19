package colang.ast.parsed.routines

import colang.ast.parsed._
import colang.ast.raw
import colang.issues.{Issue, Terms}
import colang.tokens.NativeKeyword

private[routines] object RegisterMethods {

  /**
    * "Registers" methods in their types, doesn't parse bodies.
    * @param types types to check
    * @return (new methods, encountered issues)
    */
  def registerMethods(types: Seq[Type]): (Seq[Method], Seq[Issue]) = {
    val result = types map { type_ =>
      type_.definition match {
        case Some(raw.TypeDefinition(_, _, _, raw.TypeBody(_, memberDefs, _))) =>
          val methodDefs = memberDefs flatMap {
            case f: raw.FunctionDefinition => Some(f)
            case _ => None
          }

          val methodResult = methodDefs map { registerMethod(type_, _) }
          val methods = methodResult map { _._1 }
          val methodsIssues = methodResult flatMap { _._2 }
          (methods, methodsIssues)

        case _ => (Seq.empty, Seq.empty)
      }
    }

    val methods = result flatMap { _._1 }
    val issues = result flatMap { _._2 }
    (methods, issues)
  }

  private def registerMethod(type_ : Type, methodDef: raw.FunctionDefinition): (Method, Seq[Issue]) = {
    val (returnType, returnTypeIssues) = Type.resolve(type_, methodDef.returnType)

    val localContext = LocalContext(applicableKind = Terms.Method, expectedReturnType = returnType)
    val methodBody = new CodeBlock(new LocalScope(Some(type_)), localContext, methodDef.body)

    val paramsResult = methodDef.parameterList.params map { rawParam =>
      val (paramType, paramTypeIssues) = Type.resolve(type_, rawParam.type_)
      val param = Variable(
        name = rawParam.name.value,
        scope = Some(methodBody.innerScope),
        type_ = paramType,
        definition = Some(rawParam))

      (param, paramTypeIssues)
    }

    val params = paramsResult map { _._1 }
    val paramIssues = paramsResult flatMap { _._2 }

    val method = new Method(
      name = methodDef.name.value,
      container = type_,
      returnType = returnType,
      parameters = params,
      body = methodBody,
      definition = Some(methodDef),
      native = methodDef.specifiers.has(classOf[NativeKeyword]))

    val methodIssues = type_.tryAddObjectMember(method)
    (method, returnTypeIssues ++ paramIssues ++ methodIssues)
  }
}
