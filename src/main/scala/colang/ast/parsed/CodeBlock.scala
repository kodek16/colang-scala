package colang.ast.parsed

import colang.Issue
import colang.ast.parsed.expression.Expression
import colang.ast.raw

import scala.collection.mutable.ListBuffer

/**
  * Represents a code fragment that can be executed.
  */
trait Statement

/**
  * Represents a code block enclosed in curly braces that has its own local scope.
  * @param innerScope block scope
  * @param statements statements in block
  */
class CodeBlock(var innerScope: Scope,
                val statements: ListBuffer[Statement] = ListBuffer.empty) extends Statement

object CodeBlock {

  /**
    * Analyzes the raw block and populates the CodeBlock instance that was passed as a second parameter.
    * This allows client code to parse blocks after adding some specific statements or local symbols (e.g., 'for'
    * statement).
    * @param rawBlock raw code block to analyze
    * @param block block to populate
    * @return encountered issues
    */
  def analyze(rawBlock: raw.CodeBlock, block: CodeBlock): Seq[Issue] = {
    val issues = ListBuffer.empty[Issue]

    rawBlock.statements foreach {
      case r: raw.statement.VariablesDefinition =>
        val (_, initStatements, varIssues) = VariablesDefinition.register(block.innerScope, r)
        block.statements ++= initStatements
        issues ++= varIssues

      case r: raw.CodeBlock =>
        val childBlock = new CodeBlock(new LocalScope(Some(block.innerScope)))
        val blockIssues = CodeBlock.analyze(r, childBlock)
        block.statements += childBlock
        issues ++= blockIssues

      case r: raw.expression.Expression =>
        val (parsedExpr, exprIssues) = Expression.analyze(block.innerScope, r)
        block.statements += parsedExpr
        issues ++= exprIssues
    }

    issues.toList
  }
}