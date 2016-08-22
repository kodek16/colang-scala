package colang.ast.parsed

import colang.ast.parsed.expression.Expression
import colang.ast.parsed.statement.{IfElseStatement, Statement}
import colang.ast.raw
import colang.{Error, Issue}

import scala.collection.mutable.ListBuffer

/**
  * Represents a code block enclosed in curly braces that has its own local scope.
  * For parsing code blocks first create an empty CodeBlock instance manually, then add some case-specific
  * variables and statements (function parameters, 'for' loop iterators, etc.), and then call instance methods
  * to populate the block and generate issues.
  * @param innerScope block scope
  * @param statements statements in block
  */
class CodeBlock(var innerScope: Scope,
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
      case r: raw.statement.IfStatement => addStatement(r)
      case r: raw.statement.IfElseStatement => addStatement(r)
      case r: raw.statement.VariablesDefinition => addStatement(r)
      case r: raw.CodeBlock => addStatement(r)
      case r: raw.expression.Expression => addStatement(r)
    }
  }

  def addStatement(rawStmt: raw.statement.IfStatement): Seq[Issue] = {
    val (condition, conditionIssues) = Expression.analyze(innerScope, rawStmt.condition)

    val conditionTypeIssues = if (condition.type_ == innerScope.root.resolve("bool").get) {
      Seq.empty
    } else {
      val typeStr = condition.type_.qualifiedName
      val issue = Error(rawStmt.condition.source,
        s"if statement condition must have type 'bool', but has type '$typeStr'.")
      Seq(issue)
    }

    val ifBranchBlock = new CodeBlock(new LocalScope(Some(innerScope)))
    val ifBranchIssues = ifBranchBlock.addStatement(rawStmt.ifBranch)

    statements += IfElseStatement(condition, ifBranchBlock, None)
    conditionIssues ++ conditionTypeIssues ++ ifBranchIssues
  }

  def addStatement(rawStmt: raw.statement.IfElseStatement): Seq[Issue] = {
    val (condition, conditionIssues) = Expression.analyze(innerScope, rawStmt.ifStatement.condition)

    val conditionTypeIssues = if (condition.type_ == innerScope.root.resolve("bool").get) {
      Seq.empty
    } else {
      val typeStr = condition.type_.qualifiedName
      val issue = Error(rawStmt.ifStatement.condition.source,
        s"if statement condition must have type 'bool', but has type '$typeStr'.")
      Seq(issue)
    }

    val ifBranchBlock = new CodeBlock(new LocalScope(Some(innerScope)))
    val ifBranchIssues = ifBranchBlock.addStatement(rawStmt.ifStatement.ifBranch)

    val elseBranchBlock = new CodeBlock(new LocalScope(Some(innerScope)))
    val elseBranchIssues = elseBranchBlock.addStatement(rawStmt.elseBranch)

    statements += IfElseStatement(condition, ifBranchBlock, Some(elseBranchBlock))
    conditionIssues ++ conditionTypeIssues ++ ifBranchIssues ++ elseBranchIssues
  }

  def addStatement(rawStmt: raw.statement.VariablesDefinition): Seq[Issue] = {
    val (_, initStatements, varIssues) = VariablesDefinition.register(innerScope, rawStmt)
    statements ++= initStatements
    varIssues
  }

  def addStatement(rawStmt: raw.CodeBlock): Seq[Issue] = {
    val childBlock = new CodeBlock(new LocalScope(Some(innerScope)))
    val blockIssues = childBlock.addStatementsFromBlock(rawStmt)
    statements += childBlock
    blockIssues
  }

  def addStatement(rawStmt: raw.expression.Expression): Seq[Issue] = {
    val (parsedExpr, exprIssues) = Expression.analyze(innerScope, rawStmt)
    statements += parsedExpr
    exprIssues
  }
}
