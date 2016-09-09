package colang.ast.parsed

import colang.ast.raw
import colang.issues.Terms

object Variable {

  def apply(name: String,
            scope: Some[Scope],
            type_ : Type,
            definition: Option[raw.Node]) = new Variable(name, scope, type_, definition)

  def unapply(arg: Variable): Option[(String, Type)] = Some(arg.name, arg.type_)
}

object ReferenceVariable {

  /**
    * A special extractor for variables with reference types.
    */
  def unapply(arg: Variable): Option[(String, ReferenceType)] = arg match {
    case Variable(name, rt: ReferenceType) => Some(name, rt)
    case _ => None
  }
}

/**
  * Represents a variable: a name bound to a value.
  * Note that there is an additional ReferenceVariable extractor that can be used when you care whether the variable
  * has reference type.
  * @param name variable name
  * @param scope enclosing scope
  * @param type_ variable type
  * @param definition raw variable definition
  */
class Variable private (val name: String,
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