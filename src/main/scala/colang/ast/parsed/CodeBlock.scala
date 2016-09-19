package colang.ast.parsed

import colang.ast.parsed.expression.Expression
import colang.ast.parsed.statement.{IfElseStatement, ReturnStatement, Statement, WhileStatement}
import colang.ast.raw
import colang.issues.{Issue, Issues}

import scala.collection.mutable.ListBuffer

/**
  * Represents a code block enclosed in curly braces that has its own local scope.
  * For parsing code blocks first create an empty CodeBlock instance manually, then add some case-specific
  * variables and statements (function parameters, 'for' loop iterators, etc.), and then call instance methods
  * to populate the block and generate issues.
  * @param innerScope block scope
  * @param localContext local context outside the block
  * @param statements statements in block
  */
class CodeBlock(var innerScope: Scope,
                val localContext: LocalContext,
                val rawNode: Option[raw.CodeBlock],
                val statements: ListBuffer[Statement] = ListBuffer.empty) extends Statement {

  /**
    * Modifies the block object, adding all statements from rawBlock to it.
    * @param rawBlock raw block to add statements from
    * @return encountered issues
    */
  def addStatementsFromBlock(rawBlock: raw.CodeBlock): Seq[Issue] = {
    rawBlock.statements flatMap addStatement
  }

  /**
    * Modifies the block object, adding the statement to it.
    * @param statement statement to add
    * @return encountered issues
    */
  def addStatement(statement: raw.statement.Statement): Seq[Issue] = {
    statement match {
      case r: raw.statement.IfStatement => addIfStatement(r)
      case r: raw.statement.IfElseStatement => addIfElseStatement(r)
      case r: raw.statement.WhileStatement => addWhileStatement(r)
      case r: raw.statement.ReturnStatement => addReturnStatement(r)
      case r: raw.statement.VariablesDefinition => addVariablesDefinition(r)
      case r: raw.CodeBlock => addCodeBlock(r)
      case r: raw.expression.Expression => addExpression(r)
    }
  }

  def addIfStatement(rawStmt: raw.statement.IfStatement): Seq[Issue] = {
    val (condition, conditionIssues) = analyzeConditionExpression(rawStmt.condition, "if")

    val ifBranchBlock = new CodeBlock(new LocalScope(Some(innerScope)), localContext, None)
    val ifBranchIssues = ifBranchBlock.addStatement(rawStmt.ifBranch)

    statements += IfElseStatement(condition, ifBranchBlock, None, Some(rawStmt))
    conditionIssues ++ ifBranchIssues
  }

  def addIfElseStatement(rawStmt: raw.statement.IfElseStatement): Seq[Issue] = {
    val (condition, conditionIssues) = analyzeConditionExpression(rawStmt.ifStatement.condition, "if")

    val ifBranchBlock = new CodeBlock(new LocalScope(Some(innerScope)), localContext, None)
    val ifBranchIssues = ifBranchBlock.addStatement(rawStmt.ifStatement.ifBranch)

    val elseBranchBlock = new CodeBlock(new LocalScope(Some(innerScope)), localContext, None)
    val elseBranchIssues = elseBranchBlock.addStatement(rawStmt.elseBranch)

    statements += IfElseStatement(condition, ifBranchBlock, Some(elseBranchBlock), Some(rawStmt))
    conditionIssues ++ ifBranchIssues ++ elseBranchIssues
  }

  def addWhileStatement(rawStmt: raw.statement.WhileStatement): Seq[Issue] = {
    val (condition, conditionIssues) = analyzeConditionExpression(rawStmt.condition, "while")

    val loopBlock = new CodeBlock(new LocalScope(Some(innerScope)), localContext, None)
    val loopIssues = loopBlock.addStatement(rawStmt.loop)

    statements += WhileStatement(condition, loopBlock, Some(rawStmt))
    conditionIssues ++ loopIssues
  }

  def addReturnStatement(rawStmt: raw.statement.ReturnStatement): Seq[Issue] = {
    val (returnValue, retValIssues) = rawStmt.expression match {
      case Some(rawValue) =>
        Expression.analyze(rawValue)(innerScope, localContext) match {
          case (retVal, issues) if retVal.type_ isImplicitlyConvertibleTo localContext.expectedReturnType =>
            val convertedRetVal = Type.performImplicitConversion(retVal, localContext.expectedReturnType)
            (Some(convertedRetVal), issues)

          case (retVal, issues) =>
            val actualTypeStr = retVal.type_.qualifiedName
            val expectedTypeStr = localContext.expectedReturnType.qualifiedName

            // TODO when we have methods and constructors use different issue here.
            val issue = Issues.IncompatibleFunctionReturnValue(rawStmt.source, (actualTypeStr, expectedTypeStr))
            (Some(retVal), issues :+ issue)
        }
      case None => (None, Seq.empty)
    }

    statements += ReturnStatement(returnValue, Some(rawStmt))
    retValIssues
  }

  def addVariablesDefinition(rawStmt: raw.statement.VariablesDefinition): Seq[Issue] = {
    val (_, initStatements, varIssues) = routines.registerVariables(innerScope, localContext, rawStmt)
    statements ++= initStatements
    varIssues
  }

  def addCodeBlock(rawStmt: raw.CodeBlock): Seq[Issue] = {
    val childBlock = new CodeBlock(new LocalScope(Some(innerScope)), localContext, Some(rawStmt))
    val blockIssues = childBlock.addStatementsFromBlock(rawStmt)
    statements += childBlock
    blockIssues
  }

  def addExpression(rawStmt: raw.expression.Expression): Seq[Issue] = {
    val (parsedExpr, exprIssues) = Expression.analyze(rawStmt)(innerScope, localContext)
    statements += parsedExpr
    exprIssues
  }

  /**
    * Analyzes the expression and also checks if it is eligible to be an 'if' or 'while' condition.
    * @param rawCond raw expression
    * @param statementName a string describing the enclosing statement
    * @return (parsed expression, encountered issues)
    */
  private def analyzeConditionExpression(rawCond: raw.expression.Expression,
                                         statementName: String): (Expression, Seq[Issue]) = {

    Expression.analyze(rawCond)(innerScope, localContext) match {
      case (condition, conditionIssues) if condition.type_ isImplicitlyConvertibleTo innerScope.root.boolType =>
        val convertedCondtion = Type.performImplicitConversion(condition, innerScope.root.boolType)
        (convertedCondtion, conditionIssues)

      case (condition, conditionIssues) =>
        val conditionTypeStr = condition.type_.qualifiedName
        val issue = Issues.InvalidConditionType(rawCond.source, (statementName, conditionTypeStr))
        (condition, conditionIssues :+ issue)
    }
  }
}
