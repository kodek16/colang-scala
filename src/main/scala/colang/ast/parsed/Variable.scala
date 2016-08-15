package colang.ast.parsed

import colang.SourceCode

/**
  * Represents a variable: a name bound to a value.
  * @param name variable name
  * @param declarationSite optionally variable declaration site
  * @param scope enclosing scope
  * @param type_ variable type
  */
class Variable(val name: String,
               val declarationSite: Option[SourceCode],
               val scope: Some[Scope],
               val type_ : Type) extends Symbol {

  val description = "a variable"
}
