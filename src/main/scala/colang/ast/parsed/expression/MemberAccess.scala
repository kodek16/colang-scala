package colang.ast.parsed.expression

import colang.ast.parsed._
import colang.ast.raw.{expression => raw}
import colang.issues.{Issue, Issues}

/**
  * Represents a reference to an object field.
  * Note that for a field of type 'T', instance may be either of type 'T' or of type 'T&'. This means that
  * 'instance.type_' is not always equal to 'field.container'.
  * @param instance object the field is bound to
  * @param field object field
  */
case class FieldAccess(instance: Expression,
                       field: Field,
                       rawNode: Option[raw.MemberAccess]) extends Expression {

  val type_ = (instance.type_, field.type_) match {
    case (it: NonReferenceType, ft: NonReferenceType) => ft
    case (it: NonReferenceType, ft: ReferenceType) => ft.referenced
    case (it: ReferenceType, ft: NonReferenceType) => ft.reference
    case (it: ReferenceType, ft: ReferenceType) => ft
  }
}

object MemberAccess {
  def analyze(rawExpr: raw.MemberAccess)(implicit scope: Scope): (Expression, Seq[Issue]) = {
    val (instance, instanceIssues) = Expression.analyze(scope, rawExpr.instance)

    def asFieldAccess: Option[(Expression, Seq[Issue])] = {
      val nonRefType = instance.type_ match {
        case t: NonReferenceType => t
        case rt: ReferenceType => rt.referenced
      }

      nonRefType.resolveObjectMember(rawExpr.memberName.value) match {
        case Some(f: Field) => Some((FieldAccess(instance, f, Some(rawExpr)), instanceIssues))
        case _ => None
      }
    }

    def asUnknownMemberAccess: (Expression, Seq[Issue]) = {
      val issue = Issues.UnknownObjectMember(rawExpr.memberName.source, instance.type_.qualifiedName)
      (InvalidExpression(), instanceIssues :+ issue)
    }

    asFieldAccess.getOrElse(asUnknownMemberAccess)
  }
}
