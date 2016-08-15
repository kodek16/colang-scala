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
  */
case class CBlock(heading: CSimpleStatement, variables: Seq[Variable], statements: Seq[CStatement]) extends CStatement


