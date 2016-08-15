package colang.backend.c

import colang.ast.parsed.{Method, Symbol}

/**
  * Represents a shortest structural unit in a C program.
  */
sealed trait CToken

/**
  * Represents a literal token that must be written as-is.
  * @param text token text
  */
case class CLiteralToken(text: String) extends CToken

/**
  * Represents a token referencing a stable symbol. A code writer must provide its own algorithm
  * for symbol name assignment.
  * @param symbol referenced symbol
  */
case class CSymbolReferenceToken(symbol: Symbol) extends CToken

/**
  * Represents a token referencing a method. A code writer must provide its own algorithm for method name assignment.
  * @param method referenced method
  */
case class CMethodReferenceToken(method: Method) extends CToken

/**
  * Represents an optional space that makes the code cleaner, but can be omitted for target code minification.
  */
case class COptionalSpaceToken() extends CToken