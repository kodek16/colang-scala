package colang.ast.parsed.routines

import colang.ast.parsed.{NonReferenceType, Scope}
import colang.ast.raw
import colang.issues.Issue
import colang.tokens.NativeKeyword

private[routines] object RegisterTypes {

  /**
    * "Registers" given types in the given scope. Will recursively register nested types.
    * @param scope enclosing scope
    * @param typeDefs type definitions
    * @return (new types, encountered issues)
    */
  def registerTypes(scope: Scope, typeDefs: Seq[raw.TypeDefinition]): (Seq[NonReferenceType], Seq[Issue]) = {
    val result = typeDefs map { typeDef =>
      val type_ = new NonReferenceType(
        name = typeDef.name.value,
        scope = Some(scope),
        definition = Some(typeDef),
        native = typeDef.specifiers.has(classOf[NativeKeyword]))

      val issues = scope.tryAdd(type_)

      val nestedTypeDefs = typeDef.body.members flatMap {
        case td: raw.TypeDefinition => Some(td)
        case _ => None
      }

      val (nestedTypes, nestedTypeIssues) = registerTypes(type_, nestedTypeDefs)

      (type_ +: nestedTypes, issues ++ nestedTypeIssues)
    }

    val types = result flatMap { _._1 }
    val issues = result flatMap { _._2 }
    (types, issues)
  }
}
