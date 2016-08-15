package colang.backend.c

import colang.ast.parsed.Variable

/**
  * Represents a structured C source file without assigned symbol names.
  * @param headers included headers
  * @param typeDefs C struct definitions
  * @param varDefs C global variable definitions
  * @param funcProtos C function prototypes
  * @param funcDefs C function definitions
  */
case class CSourceFile(headers: Seq[String],
                       typeDefs: Seq[CTypeDefinition],
                       varDefs: Seq[Variable],
                       funcProtos: Seq[CSimpleStatement],
                       funcDefs: Seq[CBlock])
