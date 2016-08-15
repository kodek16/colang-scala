package colang.backend.c

/**
  * Represents a C expression
  * @param tokens constituent tokens
  */
case class CExpression(tokens: Seq[CToken])