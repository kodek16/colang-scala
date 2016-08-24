package colang.ast.parsed

import colang.ast.raw

/**
  * Represents a variable: a name bound to a value.
  * @param name variable name
  * @param scope enclosing scope
  * @param type_ variable type
  * @param definition raw variable definition
  */
class Variable(val name: String,
               val scope: Some[Scope],
               val type_ : Type,
               val definition: Option[raw.Node]) extends Symbol {

  val declarationSite = definition match {
    case Some(vd: raw.statement.VariableDefinition) => Some(vd.source)
    case Some(fp: raw.FunctionParameter) => Some(fp.source)
    case _ => None
  }
  val description = "a variable"
}
