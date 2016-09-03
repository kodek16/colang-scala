package colang.backend

import colang.ast.parsed.RootNamespace

/**
  * Represents a compiler component that transforms intermediate code representation (after semantic analysis) to
  * the target representation.
  */
trait Backend {

  /**
    * Perform the transformation.
    * @param rootNamespace populated root namespace after analysis
    */
  def process(rootNamespace: RootNamespace): Unit
}
