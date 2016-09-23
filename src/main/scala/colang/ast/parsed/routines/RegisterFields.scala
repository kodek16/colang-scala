package colang.ast.parsed.routines

import colang.ast.parsed.statement.{FieldInitialization, Statement}
import colang.ast.parsed.{Field, LocalContext, NonReferenceType, Type}
import colang.ast.raw
import colang.issues.{Issue, Issues, Terms}

private[routines] object RegisterFields {

  /**
    * "Registers" fields in their respective types and generates all necessary initialization statements.
    * @param types types to check
    * @return (parsed fields, field initialization statements grouped by container type, encountered issues)
    */
  def registerFields(types: Seq[NonReferenceType]): (Seq[Field], Map[Type, Seq[Statement]], Seq[Issue]) = {
    val fieldsResults = types flatMap { type_ =>
      type_.definition.toSeq flatMap { typeDef =>
        typeDef.body.members map {
          case fieldsDef: raw.statement.VariablesDefinition => registerFieldsForType(type_, fieldsDef)
          case _ => (Seq.empty, Seq.empty, Seq.empty)
        }
      }
    }

    val fields = fieldsResults flatMap { _._1 }
    val fieldInitStatements = fieldsResults flatMap { _._2 }
    val issues = fieldsResults flatMap { _._3 }

    val groupedInitStatements = fieldInitStatements groupBy { _.field.container }

    (fields, groupedInitStatements, issues)
  }

  private def registerFieldsForType(containingType: NonReferenceType, fieldsDef: raw.statement.VariablesDefinition)
  : (Seq[Field], Seq[FieldInitialization], Seq[Issue]) = {

    val (fieldType_, fieldTypeIssues) = Type.resolve(containingType, fieldsDef.type_)

    def registerOne(fieldDef: raw.statement.VariableDefinition): (Field, Seq[FieldInitialization], Seq[Issue]) = {
      val field = new Field(
        name = fieldDef.name.value,
        container = containingType,
        type_ = fieldType_,
        definition = Some(fieldDef))

      val fieldIssues = containingType.tryAddObjectMember(field)

      implicit val initializerScope = containingType
      implicit val initializerLocalContext = LocalContext(
        applicableKind = Terms.Constructor,
        expectedReturnType = None,
        contextualObjectType = Some(containingType.reference))

      val (initStatement, initIssues) = CommonSubroutines.analyzeInitializationStatement(
        rawInitializer = fieldDef.initializer,
        objectType = field.type_,
        initializationStatementGenerator = init => FieldInitialization(field, init, None),
        incompatibleInitializerIssue = (source, initType, fieldType) => Issues.IncompatibleFieldInitializer(source, (initType, fieldType)),
        nonPlainTypeWithoutInitializerIssue = fieldType => Issues.NonPlainFieldWithoutInitializer(fieldDef.source, fieldType))

      (field, initStatement.toSeq, fieldIssues ++ initIssues)
    }

    val fieldsResults = fieldsDef.variables map registerOne

    val fields = fieldsResults map { _._1 }
    val fieldInitStatements = fieldsResults flatMap { _._2 }
    val fieldsIssues = fieldsResults flatMap { _._3 }

    (fields, fieldInitStatements, fieldTypeIssues ++ fieldsIssues)
  }
}
