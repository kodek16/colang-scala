package colang.ast.raw

import colang.SourceCode

/**
  * Represents a parsed source code fragment as a node in the abstract syntax tree (AST).
  */
trait Node {

  /**
    * Raw source code fragment.
    */
  def source: SourceCode
}