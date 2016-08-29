package colang.ast.parsed.routines

import colang.ast.parsed.{Namespace, Type}
import colang.ast.raw
import colang.issues.Issue
import colang.tokens.NativeKeyword

private[routines] object RegisterTypes {

  /**
    * "Registers" all types in the root namespace.
    * @param rootNamespace root namespace
    * @param typeDefs type definitions
    * @return (new types, encountered issues)
    */
  def registerTypes(rootNamespace: Namespace, typeDefs: Seq[raw.TypeDefinition]): (Seq[Type], Seq[Issue]) = {
    val result = for (
      typeDef <- typeDefs;

      type_ = new Type(
        name = typeDef.name.value,
        scope = Some(rootNamespace),
        definition = Some(typeDef),
        native = typeDef.specifiers.has(classOf[NativeKeyword]));

      issues = rootNamespace.tryAdd(type_)) yield (type_, issues)

    val types = result map { _._1 }
    val issues = result flatMap { _._2 }
    (types, issues)
  }
}
