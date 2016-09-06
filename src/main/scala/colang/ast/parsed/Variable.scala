package colang.ast.parsed

import colang.ast.raw
import colang.issues.Terms

object Variable {

  /**
    * A factory method for creating the correct Variable subclass.
    */
  def apply(name: String,
            scope: Some[Scope],
            type_ : Type,
            definition: Option[raw.Node]): Variable = {

    type_ match {
      case rt: ReferenceType => new ReferenceVariable(name, scope, rt, definition)
      case t: Type => new Variable(name, scope, type_, definition)
    }
  }
}

/**
  * Represents a variable: a name bound to a value.
  * @param name variable name
  * @param scope enclosing scope
  * @param type_ variable type (not a reference, reference variables have their own subclass)
  * @param definition raw variable definition
  */
class Variable protected (val name: String,
                          val scope: Some[Scope],
                          val type_ : Type,
                          val definition: Option[raw.Node]) extends Symbol {

  val definitionSite = definition match {
    case Some(vd: raw.statement.VariableDefinition) => Some(vd.source)
    case Some(fp: raw.FunctionParameter) => Some(fp.source)
    case _ => None
  }

  val description = Terms.Variable
}

/**
  * A special subclass for reference variables (that have types like 'int&' and 'double&').
  * @param name variable name
  * @param scope enclosing scope
  * @param type_ variable type (must be a reference)
  * @param definition raw variable definition
  */
class ReferenceVariable (name: String,
                         scope: Some[Scope],
                         type_ : ReferenceType,
                         definition: Option[raw.Node]) extends Variable(name, scope, type_, definition)