package colang.ast.parsed.routines

import colang.ast.parsed._
import colang.ast.raw
import colang.issues.{Issue, Terms}
import colang.tokens.{NativeKeyword, StaticKeyword}

private[routines] object RegisterMethods {

  /**
    * "Registers" methods in their types, doesn't parse bodies.
    * @param types types to check
    * @return (new methods, encountered issues)
    */
  def registerMethods(types: Seq[NonReferenceType]): (Seq[Method], Seq[Issue]) = {
    val result = types map { type_ =>
      type_.definition match {
        case Some(raw.TypeDefinition(_, _, _, raw.TypeBody(_, memberDefs, _))) =>
          val methodDefs = memberDefs flatMap {
            case f: raw.FunctionDefinition if !f.specifiers.has(classOf[StaticKeyword]) => Some(f)
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

  private def registerMethod(type_ : NonReferenceType, methodDef: raw.FunctionDefinition): (Method, Seq[Issue]) = {
    val (returnType, returnTypeIssues) = Type.resolve(methodDef.returnType)(type_)

    val containerType = if (methodDef.referenceMarker.isDefined) {
      type_.reference
    } else {
      type_
    }

    val localContext = LocalContext(
      applicableKind = Terms.Method,
      expectedReturnType = Some(returnType),
      contextualObjectType = Some(containerType))

    // Note that the method scope for method of type 'T&' has the type scope of 'T' as its parent.
    val methodBody = new CodeBlock(new LocalScope(Some(type_)), localContext, methodDef.body)

    val paramsResult = methodDef.parameterList.params map { rawParam =>
      val (paramType, paramTypeIssues) = Type.resolve(rawParam.type_)(type_)
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
      container = containerType,
      returnType = returnType,
      parameters = params,
      body = methodBody,
      definition = Some(methodDef),
      native = methodDef.specifiers.has(classOf[NativeKeyword]))

    val methodIssues = containerType.tryAddObjectMember(method)
    (method, returnTypeIssues ++ paramIssues ++ methodIssues)
  }
}
