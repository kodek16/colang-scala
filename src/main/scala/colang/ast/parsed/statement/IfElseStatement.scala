package colang.ast.parsed.statement

import colang.ast.parsed.CodeBlock
import colang.ast.parsed.expression.Expression
import colang.ast.raw.{statement => raw}

/**
  * Represents a conditional statement with either one or two branches.
  * @param condition condition to check
  * @param ifBranch statement that is executed if the condition is true
  * @param elseBranch optional statement that is executed if the condition is false
  */
case class IfElseStatement(condition: Expression,
                           ifBranch: CodeBlock,
                           elseBranch: Option[CodeBlock],
                           rawNode: Option[raw.Statement]) extends Statement
