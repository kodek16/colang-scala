package colang.ast.parsed.routines

import colang.ast.parsed._
import colang.ast.parsed.statement.{Statement, VariableInitialization}
import colang.ast.raw.{statement => raw}
import colang.issues.{Issue, Issues}

private[routines] object RegisterVariables {

  /**
    * "Registers" variables in the enclosing scope and generates all necessary initialization statements.
    * @param scope variable scope
    * @param localContext enclosing local context
    * @param varDefs raw variables definition node
    * @return (new variables, initialization statements, encountered issues)
    */
  def registerVariables(scope: Scope, localContext: LocalContext, varDefs: raw.VariablesDefinition)
      : (Seq[Variable], Seq[Statement], Seq[Issue]) = {

    val (type_, typeIssues) = Type.resolve(scope, varDefs.type_)

    def registerOne(varDef: raw.VariableDefinition): (Variable, Seq[Statement], Seq[Issue]) = {
      val variable = Variable(
        name = varDef.name.value,
        scope = Some(scope),
        type_ = type_,
        definition = Some(varDef))

      val varIssues = scope.tryAdd(variable)

      implicit val initializerScope = scope
      implicit val initializerLocalContext = localContext

      val (initStatement, initIssues) = CommonSubroutines.analyzeInitializationStatement(
        rawInitializer = varDef.initializer,
        objectType = variable.type_,
        initializationStatementGenerator = init => VariableInitialization(variable, init, None),
        incompatibleInitializerIssue = (source, initType, varType) => Issues.IncompatibleVariableInitializer(source, (initType, varType)),
        nonPlainTypeWithoutInitializerIssue = varType => Issues.NonPlainVariableWithoutInitializer(varDef.source, varType))

      (variable, initStatement.toSeq, varIssues ++ initIssues)
    }

    val result = varDefs.variables map registerOne
    val variables = result map { _._1 }
    val initStatements = result flatMap { _._2 }
    val varIssues = result flatMap { _._3 }

    (variables, initStatements, typeIssues ++ varIssues)
  }
}
