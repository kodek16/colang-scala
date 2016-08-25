package colang.backend.c

import colang.ast.parsed.Variable

/**
  * Represents a C statement.
  */
sealed trait CStatement

/**
  * Represents a simple one-line statement.
  * @param tokens constituent tokens
  */
case class CSimpleStatement(tokens: Seq[CToken]) extends CStatement

/**
  * Represents a C code block
  * @param heading tokens to go before the block (like 'if' or 'while' statements)
  * @param variables variables declared in the block
  * @param statements statements in the block
  * @param tail statement to be written after the closing brace (can be a whole block, like 'else' branch).
  */
case class CBlock(heading: Seq[CToken],
                  variables: Seq[Variable],
                  statements: Seq[CStatement],
                  tail: Option[CStatement] = None) extends CStatement