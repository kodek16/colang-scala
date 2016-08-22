package colang.ast.parsed

import colang.SourceCode

class Namespace(val name: String,
                val declarationSite: Option[SourceCode],
                val parent: Option[Namespace]) extends Symbol with Scope {

  val scope = parent
  val description = "a namespace"

  /**
    * A type assigned to values that failed analysis.
    */
  val unknownType: Type = new Type(
    name = "(unknown type)",
    declarationSite = None,
    scope = Some(this))
}

