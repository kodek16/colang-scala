package colang.ast.parsed

import colang.ast.raw
import colang.issues.Terms

/**
  * Represents an object field: a piece of data associated with an object.
  * @param name field name
  * @param container associated object type
  * @param type_ field type
  */
class Field(val name: String,
            val container: Type,
            val type_ : Type,
            val definition: Option[raw.statement.VariableDefinition]) extends ObjectMember {

  val description = Terms.Field

  val definitionSite = definition match {
    case Some(vd) => Some(vd.source)
    case None => None
  }
}
