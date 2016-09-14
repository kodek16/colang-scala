package colang.ast.parsed.routines

import colang.ast.parsed.{Field, Type}
import colang.ast.raw
import colang.issues.Issue

private[routines] object RegisterFields {

  /**
    * "Registers" fields in their respective types.
    * @param types types to check
    * @return encountered issues
    */
  def registerFields(types: Seq[Type]): Seq[Issue] = {
    types flatMap { type_ =>
      type_.definition.toSeq flatMap { typeDef =>
        typeDef.body.members flatMap {
          case fieldsDef: raw.statement.VariablesDefinition => registerField(type_, fieldsDef)
          case _ => Seq.empty
        }
      }
    }
  }

  def registerField(containingType : Type, fieldsDef: raw.statement.VariablesDefinition): Seq[Issue] = {
    val (fieldType_, fieldTypeIssues) = Type.resolve(containingType, fieldsDef.type_)

    val fieldsIssues = fieldsDef.variables flatMap { fieldDef =>
      val field = new Field(
        name = fieldDef.name.value,
        container = containingType,
        type_ = fieldType_,
        definition = Some(fieldDef))

      val fieldIssues = containingType.tryAddObjectMember(field)

      fieldIssues
    }

    fieldTypeIssues ++ fieldsIssues
  }
}
