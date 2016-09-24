package colang.ast.parsed.routines

import colang.ast.parsed._
import colang.ast.raw
import colang.issues.{Issue, Terms}
import colang.tokens.NativeKeyword

private[routines] object RegisterConstructors {

  /**
    * "Registers" constructors in their types, doesn't parse bodies.
    * @param types types to check
    * @return (new constructors, encountered issues)
    */
  def registerConstructors(types: Seq[NonReferenceType]): (Seq[Constructor], Seq[Issue]) = {
    val result = types map { type_ =>
      type_.definition match {
        case Some(raw.TypeDefinition(_, _, _, raw.TypeBody(_, memberDefs, _))) =>
          val constructorDefs = memberDefs flatMap {
            case c: raw.ConstructorDefinition => Some(c)
            case _ => None
          }

          val constructorResult = constructorDefs map { registerConstructor(type_, _) }
          val constructors = constructorResult map { _._1 }
          val constructorsIssues = constructorResult flatMap { _._2 }
          (constructors, constructorsIssues)

        case _ => (Seq.empty, Seq.empty)
      }
    }

    val constructors = result flatMap { _._1 }
    val issues = result flatMap { _._2 }
    (constructors, issues)
  }

  private def registerConstructor(type_ : NonReferenceType, constructorDef: raw.ConstructorDefinition): (Constructor, Seq[Issue]) = {
    val localContext = LocalContext(
      applicableKind = Terms.Constructor,
      expectedReturnType = None,
      contextualObjectType = Some(type_.reference))

    val constructorBody = new CodeBlock(new LocalScope(Some(type_)), localContext, constructorDef.body)

    val paramsResult = constructorDef.parameterList.params map { rawParam =>
      val (paramType, paramTypeIssues) = Type.resolve(rawParam.type_)(type_)
      val param = Variable(
        name = rawParam.name.value,
        scope = Some(constructorBody.innerScope),
        type_ = paramType,
        definition = Some(rawParam))

      (param, paramTypeIssues)
    }

    val params = paramsResult map { _._1 }
    val paramIssues = paramsResult flatMap { _._2 }

    val constructor = new Constructor(
      type_ = type_,
      parameters = params,
      body = constructorBody,
      definition = Some(constructorDef),
      native = constructorDef.specifiers.has(classOf[NativeKeyword]))

    val constructorIssues = type_.tryAddConstructor(constructor)
    (constructor, paramIssues ++ constructorIssues)
  }
}
