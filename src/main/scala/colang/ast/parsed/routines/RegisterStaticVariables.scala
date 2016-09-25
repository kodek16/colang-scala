package colang.ast.parsed.routines

import colang.ast.parsed.statement.Statement
import colang.ast.parsed.{LocalContext, Type, Variable}
import colang.issues.{Issue, Terms}
import colang.ast.raw
import colang.tokens.StaticKeyword

private[routines] object RegisterStaticVariables {

  /**
    * "Registers" static variables in their types and generates necessary initialization statements.
    * @param types types to analyze
    * @return (static variables, initialization statements, encountered issues)
    */
  def registerStaticVariables(types: Seq[Type]): (Seq[Variable], Seq[Statement], Seq[Issue]) = {
    val result = types flatMap { type_ =>
      val initializerLocalContext = LocalContext(
        Terms.Function,
        expectedReturnType = Some(type_.scope.get.root.voidType))

      type_.definition.toSeq flatMap { typeDef =>
        typeDef.body.members flatMap {
          case varsDef: raw.statement.VariablesDefinition if varsDef.specifiers.has(classOf[StaticKeyword]) =>
            Seq(RegisterVariables.registerVariables(type_, initializerLocalContext, varsDef))
          case _ => Seq.empty
        }
      }
    }

    val variables = result flatMap { _._1 }
    val initializationStatements = result flatMap { _._2 }
    val issues = result flatMap { _._3 }

    (variables, initializationStatements, issues)
  }
}
