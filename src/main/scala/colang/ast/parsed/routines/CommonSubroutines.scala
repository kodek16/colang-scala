package colang.ast.parsed.routines

import colang.SourceCode
import colang.ast.parsed.expression.{ConstructorCall, Expression}
import colang.ast.parsed.statement.Statement
import colang.ast.parsed.{LocalContext, Scope, Type}
import colang.ast.raw.{expression => raw}
import colang.issues.Issue

private[routines] object CommonSubroutines {

  /**
    * Analyzes a raw initializer expression.
    * If no initializer was provided, will try to use the default constructor call as an initializer, and will
    * generate issues on failure.
    * @param rawInitializer raw initializer expression
    * @param objectType type of the object (variable/field) that the initializer belongs to
    * @param initializationStatementGenerator a function that actually creates an initializer from a parsed expression
    * @param incompatibleInitializerIssue an issue generator for incompatible initializers, the arguments are
    *                                     (initializer source, initializer type name, object type name)
    * @param nonPlainTypeWithoutInitializerIssue an issue generator for a non-plain-typed object without an initializer,
    *                                            accepts the object type
    * @return (Some(initialization statement) on success, encountered issues)
    */
  def analyzeInitializationStatement[IST <: Statement](rawInitializer: Option[raw.Expression],
                                                       objectType: Type,
                                                       initializationStatementGenerator: Expression => IST,
                                                       incompatibleInitializerIssue: (SourceCode, String, String) => Issue,
                                                       nonPlainTypeWithoutInitializerIssue: String => Issue)
                                                      (implicit scope: Scope, localContext: LocalContext)
  : (Option[IST], Seq[Issue]) = {

    rawInitializer match {
      case Some(rawInit) =>
        val (init, initIssues) = Expression.analyze(rawInit)(scope, localContext)

        val (initStatement, initTypeIssues) = init.type_ match {
          case t if t.isImplicitlyConvertibleTo(objectType) =>
            val convertedInit = Type.performImplicitConversion(init, objectType)
            val statement = initializationStatementGenerator(convertedInit)
            (Some(statement), Seq.empty)

          case _ =>
            val initTypeStr = init.type_.qualifiedName
            val varTypeStr = objectType.qualifiedName
            val issue = incompatibleInitializerIssue(rawInit.source, initTypeStr, varTypeStr)
            (None, Seq(issue))
        }

        (initStatement, initIssues ++ initTypeIssues)

      case None =>
        objectType.defaultConstructor match {
          case Some(defaultCtor) =>
            val initializer = ConstructorCall(defaultCtor, Seq.empty, None)
            val statement = initializationStatementGenerator(initializer)
            (Some(statement), Seq.empty)

          case None =>
            val issue = nonPlainTypeWithoutInitializerIssue(objectType.qualifiedName)
            (None, Seq(issue))
        }
    }
  }
}
