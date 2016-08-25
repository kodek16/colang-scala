package colang.ast.parsed.statement

import colang.ast.raw

/**
  * Represents a code fragment that can be executed.
  */
trait Statement {

  /**
    * Raw statement node.
    */
  def rawNode: Option[raw.Node]
}