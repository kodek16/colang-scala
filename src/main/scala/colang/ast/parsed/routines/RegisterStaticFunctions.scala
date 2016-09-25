package colang.ast.parsed.routines

import colang.ast.parsed.{Function, Type}
import colang.ast.raw
import colang.issues.Issue
import colang.tokens.StaticKeyword

private[routines] object RegisterStaticFunctions {

  /**
    * "Registers" static functions in their types.
    * @param types types to analyze
    * @return (static functions, encountered issues)
    */
  def registerStaticFunctions(types: Seq[Type]): (Seq[Function], Seq[Issue]) = {
    val result = types flatMap { type_ =>
      type_.definition.toSeq flatMap { typeDef =>
        val staticFunctionDefs = typeDef.body.members flatMap {
          case funcDef: raw.FunctionDefinition if funcDef.specifiers.has(classOf[StaticKeyword]) => Some(funcDef)
          case _ => None
        }

        Seq(RegisterFunctions.registerFunctions(type_, staticFunctionDefs))
      }
    }

    val functions = result flatMap { _._1 }
    val issues = result flatMap { _._2 }

    (functions, issues)
  }
}
