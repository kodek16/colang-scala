package colang.backend.c

import colang.ast.parsed.Type

/**
  * Represents a C struct definition.
  * @param type_ defined type
  */
case class CTypeDefinition(type_ : Type)
