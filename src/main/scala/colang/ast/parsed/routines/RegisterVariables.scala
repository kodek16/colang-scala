package colang.ast.parsed.routines

import colang.ast.parsed._
import colang.ast.parsed.expression.Expression
import colang.ast.parsed.statement.{Statement, VariableConstructorCall}
import colang.ast.raw.{statement => raw}
import colang.issues.{Issue, Issues}

private[routines] object RegisterVariables {

  /**
    * "Registers" variables in the enclosing scope and generates all necessary initialization statements.
    * @param scope variable scope
    * @param localContext enclosing local context
    * @param rawDefs raw variables definition node
    * @return (new variables, initialization statements, encountered issues)
    */
  def registerVariables(scope: Scope, localContext: LocalContext, rawDefs: raw.VariablesDefinition)
      : (Seq[Variable], Seq[Statement], Seq[Issue]) = {

    val (type_, typeIssues) = Type.resolve(scope, rawDefs.type_)

    def registerOne(rawDef: raw.VariableDefinition): (Variable, Seq[Statement], Seq[Issue]) = {
      val variable = Variable(
        name = rawDef.name.value,
        scope = Some(scope),
        type_ = type_,
        definition = Some(rawDef))

      val varIssues = scope.tryAdd(variable)

      val (initStatement, initIssues) = rawDef.initializer match {
        case Some(rawInit) =>
          val (init, initIssues) = Expression.analyze(rawInit)(scope, localContext)
          val copyConstructor = variable.type_.copyConstructor

          if (copyConstructor.canBeAppliedTo(Seq(init.type_))) {
            val constructorArgs = Type.performImplicitConversions(Seq(init), copyConstructor.parameters map { _.type_ })

            val constructStatement = VariableConstructorCall(
              copyConstructor,
              variable,
              constructorArgs,
              None)

            (Some(constructStatement), initIssues)

          } else {
            val initTypeStr = init.type_.qualifiedName
            val varTypeStr = variable.type_.qualifiedName
            val issue = Issues.IncompatibleVariableInitializer(rawInit.source, (initTypeStr, varTypeStr))
            (None, initIssues :+ issue)
          }

        case None =>
          variable.type_.defaultConstructor match {
            case Some(defaultConstructor) =>
              val constructStatement = VariableConstructorCall(
                defaultConstructor,
                variable,
                Seq.empty,
                None)

              (Some(constructStatement), Seq.empty)

            case None =>
              val issue = Issues.NonPlainVariableWithoutInitializer(rawDef.source, variable.type_.qualifiedName)
              (None, Seq(issue))
          }
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
