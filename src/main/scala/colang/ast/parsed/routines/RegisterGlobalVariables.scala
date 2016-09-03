package colang.ast.parsed.routines

import colang.ast.parsed.statement.Statement
import colang.ast.parsed.{RootNamespace, Variable}
import colang.ast.raw.{statement => raw}
import colang.issues.Issue

private[routines] object RegisterGlobalVariables {

  /**
    * Applies registerVariables to all global variable defintions
    * @param rootNamespace root namespace
    * @param rawDefs raw variables definition nodes
    * @return (new variables, initialization statements, encountered issues)
    */
  def registerGlobalVariables(rootNamespace: RootNamespace,
                              rawDefs: Seq[raw.VariablesDefinition]): (Seq[Variable], Seq[Statement], Seq[Issue]) = {

    val result = rawDefs map { RegisterVariables.registerVariables(rootNamespace, _ )}
    val variables = result flatMap { _._1 }
    val initStatements = result flatMap { _._2 }
    val issues = result flatMap { _._3 }
    (variables, initStatements, issues)
  }
}
