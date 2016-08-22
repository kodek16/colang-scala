package colang.ast.parsed.statement

import colang.ast.parsed.CodeBlock
import colang.ast.parsed.expression.Expression

/**
  * Represents a 'while' loop.
  * @param condition loop condition
  * @param loop loop body
  */
case class WhileStatement(condition: Expression, loop: CodeBlock) extends Statement