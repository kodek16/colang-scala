package colang.ast.parsed.statement

import colang.ast.parsed.CodeBlock
import colang.ast.parsed.expression.Expression
import colang.ast.raw.{statement => raw}

/**
  * Represents a 'while' loop.
  * @param condition loop condition
  * @param loop loop body
  */
case class WhileStatement(condition: Expression,
                          loop: CodeBlock,
                          rawNode: Option[raw.Statement]) extends Statement
