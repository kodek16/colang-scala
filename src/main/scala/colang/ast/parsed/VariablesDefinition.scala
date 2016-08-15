package colang.ast.parsed

import colang.ast.parsed.expression.{Expression, MethodCall, VariableReference}
import colang.ast.raw.{ statement => raw }
import colang.{Error, Issue}

object VariablesDefinition {

  /**
    * Tries to add variables to the enclosing scope and generates initialization statements (if any).
    * @param scope enclosing scope
    * @param rawDefs raw variable definition statement
    * @return (new variable objects (possibly detached), initialization statements, encountered issues)
    */
  def register(scope: Scope, rawDefs: raw.VariablesDefinition): (Seq[Variable], Seq[Statement], Seq[Issue]) = {
    val (type_, typeIssues) = Type.resolve(scope, rawDefs.type_)

    def registerOne(rawDef: raw.VariableDefinition): (Variable, Option[Statement], Vector[Issue]) = {
      val variable = new Variable(
        name = rawDef.name.value,
        declarationSite = Some(rawDef.source),
        scope = Some(scope),
        type_ = type_)

      val (initStatement, initIssues) = rawDef.initializer match {
        case Some(rawInit) =>
          val (init, initIssues) = Expression.analyze(scope, rawInit)

          val assignMethod = variable.type_.resolveMethod("assign").get

          if (assignMethod.canBeAppliedTo(Seq(init.type_))) {
            (Some(MethodCall(assignMethod, VariableReference(variable), Seq(init))), initIssues)
          } else {
            val initTypeStr = init.type_.qualifiedName
            val varTypeStr = variable.type_.qualifiedName
            val issue = Error(rawInit.source,
              s"initializer has type '$initTypeStr', which is incompatible with variable type '$varTypeStr'")

            (None, initIssues :+ issue)
          }
        case None => (None, Seq.empty)
      }

      (variable, initStatement, scope.tryAdd(variable).toVector ++ initIssues)
    }

    val results = rawDefs.variables map registerOne
    val variables = results map { _._1 }
    val initStatements = results flatMap { _._2 }
    val issues = typeIssues ++ (results flatMap { _._3 })

    (variables, initStatements, issues)
  }
}
