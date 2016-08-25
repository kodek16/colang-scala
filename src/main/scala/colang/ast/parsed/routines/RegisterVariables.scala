package colang.ast.parsed.routines

import colang.ast.parsed.expression.{Expression, MethodCall, VariableReference}
import colang.ast.parsed.statement.Statement
import colang.ast.parsed.{Scope, Type, Variable}
import colang.ast.raw.{statement => raw}
import colang.{Error, Issue}

private[routines] object RegisterVariables {

  /**
    * "Registers" variables in the enclosing scope and generates all necessary initialization statements.
    * @param scope variable scope
    * @param rawDefs raw variables definition node
    * @return (new variables, initialization statements, encountered issues)
    */
  def registerVariables(scope: Scope, rawDefs: raw.VariablesDefinition): (Seq[Variable], Seq[Statement], Seq[Issue]) = {
    val (type_, typeIssues) = Type.resolve(scope, rawDefs.type_)

    def registerOne(rawDef: raw.VariableDefinition): (Variable, Seq[Statement], Seq[Issue]) = {
      val variable = new Variable(
        name = rawDef.name.value,
        scope = Some(scope),
        type_ = type_,
        definition = Some(rawDef))

      val varIssues = scope.tryAdd(variable).toSeq

      val (initStatement, initIssues) = rawDef.initializer match {
        case Some(rawInit) =>
          val (init, initIssues) = Expression.analyze(scope, rawInit)

          val assignMethod = variable.type_.resolveMethod("assign").get

          if (assignMethod.canBeAppliedTo(Seq(init.type_))) {
            val initStatement = MethodCall(assignMethod, VariableReference(variable, None), Seq(init), Some(rawDef))
            (Some(initStatement), initIssues)

          } else {
            val initTypeStr = init.type_.qualifiedName
            val varTypeStr = variable.type_.qualifiedName
            val issue = Error(rawInit.source,
              s"initializer has type '$initTypeStr', which is incompatible with variable type '$varTypeStr'")

            (None, initIssues :+ issue)
          }
        case None => (None, Seq.empty)
      }

      (variable, initStatement.toSeq, varIssues ++ initIssues)
    }

    val result = rawDefs.variables map registerOne
    val variables = result map { _._1 }
    val initStatements = result flatMap { _._2 }
    val varIssues = result flatMap { _._3 }

    (variables, initStatements, typeIssues ++ varIssues)
  }
}
